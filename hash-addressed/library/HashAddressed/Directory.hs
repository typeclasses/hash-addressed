module HashAddressed.Directory
  (
    {- * Type -} Directory (..),
    {- * Write operations -}
            writeLazy, writeStream, writeExcept,
            WriteResult (..), WriteType (..),
  )
  where

import Essentials
import HashAddressed.HashFunction

import System.FilePath ((</>))

import qualified Data.Either as Either
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans as Monad
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString as Strict
import qualified Data.ByteString as Strict.ByteString
import qualified Data.ByteString.Lazy as Lazy
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified System.IO.Temp as Temporary
import qualified Fold.Effectful as Fold

{-| Specification of a hash-addressed directory

Note that the utilities in "HashAddressed.Directory" do not create the
directory; ensure that it already exists before attempting to write.

See "HashAddressed.HashFunction" for examples of hash functions. -}
data Directory = Directory
  { directoryPath :: IO.FilePath
      {- ^ Directory where hash-addressed files are stored -}
  , hashFunction :: HashFunction
      {- ^ Hash function to use for generating file names -}
  }

data WriteResult = WriteResult
  { hashAddressedFile :: IO.FilePath
      {- ^ The file path where the contents written by the
           action now reside, including the store directory -}
  , writeType :: WriteType
  }

data WriteType =
    AlreadyPresent -- ^ No action was taken because the content
                   --   is already present in the directory
  | NewContent     -- ^ A new file was written into the directory

{-| Path of a file that we write to before moving it into the
    hash-addressed directory -}
newtype TemporaryFile = TemporaryFile{ temporaryFilePath :: IO.FilePath }

{-| File path within a hash-addressed directory

This does no include the directory part, just the file name. -}
newtype HashName = HashName IO.FilePath

{-| Write a stream of strict ByteStrings to a hash-addressed directory,
    possibly aborting mid-stream with an error value instead

If the producer throws @abort@ or an 'IO.IO' exception, nothing will be written.
An @abort@ thrown via 'Except.ExceptT' will be re-thrown via 'Except.MonadError',
and an exception thrown via 'IO.IO' will be re-thrown via 'IO.IO'. -}
writeExcept :: forall abort commit m. (IO.MonadIO m, Except.MonadError abort m) =>
    Directory -- ^ Where to write
    -> Pipes.Producer Strict.ByteString (Except.ExceptT abort IO.IO) commit
        -- ^ What to write
    -> m (commit, WriteResult)
writeExcept dir stream = (liftExceptIO . Except.ExceptT . Resource.runResourceT) do

    {-  Where the system in general keeps its temporary files  -}
    temporaryRoot <- Monad.liftIO Temporary.getCanonicalTemporaryDirectory

    {-  We do not yet know what the final file path will be, because that is
        determined by the hash of the contents, which we have not computed yet. -}

    {-  We will write the file into this directory and then move it out in an
        atomic rename operation that will commit the file to the store.  -}
    (_, temporaryDirectory) <- Resource.allocate
        (Temporary.createTempDirectory temporaryRoot "hash-addressed")
        Directory.removeDirectoryRecursive {- (🧹) -}

    {-  If the file never gets moved, then when the directory is removed
        recursively (🧹), the file will be destroyed along with it.

        If the file does get moved, the directory will be destroyed (🧹),
        but the file, which no longer resides within the directory, will remain. -}

    {-  The path of the file we're writing, in its temporary location  -}
    let temporaryFile = TemporaryFile (temporaryDirectory </> "hash-addressed-file")

    {-  Create the file and open a handle to write to it  -}
    (handleRelease, handle) <- Resource.allocate
        (IO.openBinaryFile (temporaryFilePath temporaryFile) IO.WriteMode)
        IO.hClose {- (🍓) -}

    {-  Run the continuation, doing two things at once with the ByteString
        chunks it gives us: write the file, and update a hash context -}
    abortOrCommit :: Either.Either abort (HashName, commit) <-
        Monad.lift $ runStream (hashFunction dir) handle stream

    {-  Once we're done writing the file, we no longer need the handle.  -}
    Resource.release handleRelease {- (🍓) -}

    case abortOrCommit of
        Either.Left abort -> pure (Either.Left abort)
        Either.Right (name, commit) -> do
            result <- finalize dir temporaryFile name
            pure $ Either.Right (commit, result)

finalize :: Pipes.MonadIO m => Directory -> TemporaryFile -> HashName -> m WriteResult
finalize dir temporaryFile (HashName name) = do

    let hashAddressedFile = directoryPath dir </> name

    -- Another file of the same name in the content-addressed directory might already exist.
    writeType <- Monad.liftIO $
          Directory.doesPathExist hashAddressedFile
          <&> \case{ True -> AlreadyPresent; False -> NewContent }

    case writeType of

        -- In one atomic step, this action commits the file to the store and prevents it
        -- from being deleted by the directory cleanup action (🧹).
        NewContent -> Monad.liftIO $
            Directory.renamePath (temporaryFilePath temporaryFile) hashAddressedFile

        -- Since the store is content-addressed, we assume that two files with the same
        -- name have the same contents. Therefore, if a file already exists at this path,
        -- there is no reason to take any action.
        AlreadyPresent -> pure ()

    pure WriteResult{ hashAddressedFile, writeType }

runStream :: forall abort commit. HashFunction -> IO.Handle
    -> Pipes.Producer Strict.ByteString (Except.ExceptT abort IO.IO) commit
    -> IO.IO (Either.Either abort (HashName, commit))
runStream hash handle stream =
    case writeAndHash hash handle of
        Fold.EffectfulFold{ Fold.initial, Fold.step, Fold.extract } ->
            Except.runExceptT $
                Pipes.foldM' step initial extract stream

writeAndHash :: HashFunction -> IO.Handle
    -> Fold.EffectfulFold (Except.ExceptT abort IO.IO) Strict.ByteString HashName
writeAndHash (HashFunction hash) handle =
    (Fold.effect \chunk -> Monad.liftIO (Strict.ByteString.hPut handle chunk))
    *> (HashName <$> Fold.fold hash)

liftExceptIO :: forall e m a. (IO.MonadIO m, Except.MonadError e m) =>
    Except.ExceptT e IO.IO a -> m a
liftExceptIO x = do
    r <- Except.runExceptT (Except.mapExceptT Monad.liftIO x)
    Except.liftEither r

{-| Write a stream of strict ByteStrings to a hash-addressed directory

If the producer throws an exception, nothing will be written and the
exception will be re-thrown.

This is a simplified variant of 'writeExcept'. -}
writeStream :: forall commit m. IO.MonadIO m =>
    Directory -- ^ Where to write
    -> Pipes.Producer Strict.ByteString IO.IO commit -- ^ What to write
    -> m (commit, WriteResult)
writeStream dir source = voidExcept $ writeExcept dir (Pipes.hoist Monad.lift source)

voidLeft :: Either.Either Void a -> a
voidLeft = \case{ Either.Left x -> absurd x; Either.Right x -> x }

voidExcept :: Functor m => Except.ExceptT Void m a -> m a
voidExcept = Except.runExceptT >>> fmap voidLeft

{-| Write a lazy ByteString to a hash-addressed directory

This is a simplified variant of 'writeStream'. -}
writeLazy :: forall m. IO.MonadIO m =>
    Directory -- ^ Where to write
    -> Lazy.ByteString -- ^ What to write
    -> m WriteResult
writeLazy dir lbs = writeStream dir (lbsProducer lbs) <&> (\((), x) -> x)

lbsProducer :: Lazy.ByteString -> Pipes.Producer Strict.ByteString IO.IO ()
lbsProducer = Lazy.toChunks >>> Pipes.each
