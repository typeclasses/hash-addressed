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

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Either (Either)
import Pipes (Producer)
import System.FilePath ((</>))
import System.IO (IO, FilePath, Handle)

import qualified Control.Monad.Except as Except (liftEither, runExceptT)
import qualified Control.Monad.Trans.Resource as Resource (runResourceT, allocate, release)
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString as Strict.ByteString (hPut)
import qualified Data.ByteString.Lazy as Lazy (ByteString, toChunks)
import qualified Data.Either as Either (Either (Left, Right))
import qualified Fold.Effectful as Fold (EffectfulFold (..), effect, fold)
import qualified Pipes (hoist, each)
import qualified Pipes.Prelude as Pipes (foldM')
import qualified System.Directory as Directory (removeDirectoryRecursive, doesPathExist, renamePath)
import qualified System.IO as IO (openBinaryFile, IOMode (..), hClose)
import qualified System.IO.Temp as Temporary (getCanonicalTemporaryDirectory, createTempDirectory)

{-| Specification of a hash-addressed directory

Note that the utilities in "HashAddressed.Directory" do not create the
directory; ensure that it already exists before attempting to write.

See "HashAddressed.HashFunction" for examples of hash functions. -}
data Directory = Directory
  { directoryPath :: FilePath
      {- ^ Directory where hash-addressed files are stored -}
  , hashFunction :: HashFunction
      {- ^ Hash function to use for generating file names -}
  }

data WriteResult = WriteResult
  { hashAddressedFile :: FilePath
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
newtype TemporaryFile = TemporaryFile{ temporaryFilePath :: FilePath }

{-| File path within a hash-addressed directory

This does no include the directory part, just the file name. -}
newtype HashName = HashName FilePath

{-| Write a stream of strict ByteStrings to a hash-addressed directory,
    possibly aborting mid-stream with an error value instead

If the producer throws @abort@ or an 'IO' exception, nothing will be written.
An @abort@ thrown via 'Except.ExceptT' will be re-thrown via 'Except.MonadError',
and an exception thrown via 'IO' will be re-thrown via 'IO'. -}
writeExcept :: forall abort commit m. (MonadIO m, MonadError abort m) =>
    Directory -- ^ Where to write
    -> Pipes.Producer Strict.ByteString (ExceptT abort IO) commit
        -- ^ What to write
    -> m (commit, WriteResult)
writeExcept dir stream = run action
  where
    run = Resource.runResourceT >>> liftIO >>> (>>= Except.liftEither)

    action :: ResourceT IO (Either abort (commit, WriteResult))
    action = do
        {-  Where the system in general keeps its temporary files  -}
        temporaryRoot <- liftIO Temporary.getCanonicalTemporaryDirectory

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
        abortOrCommit :: Either abort (HashName, commit) <-
            lift $ runStream (hashFunction dir) handle stream

        {-  Once we're done writing the file, we no longer need the handle.  -}
        Resource.release handleRelease {- (🍓) -}

        case abortOrCommit of
            Either.Left abort -> pure (Either.Left abort)
            Either.Right (name, commit) -> do
                result <- finalize dir temporaryFile name
                pure $ Either.Right (commit, result)

finalize :: MonadIO m => Directory -> TemporaryFile -> HashName -> m WriteResult
finalize dir temporaryFile (HashName name) = do

    let hashAddressedFile = directoryPath dir </> name

    -- Another file of the same name in the content-addressed directory might already exist.
    writeType <- liftIO $
          Directory.doesPathExist hashAddressedFile
          <&> \case{ True -> AlreadyPresent; False -> NewContent }

    case writeType of

        -- In one atomic step, this action commits the file to the store and prevents it
        -- from being deleted by the directory cleanup action (🧹).
        NewContent -> liftIO $
            Directory.renamePath (temporaryFilePath temporaryFile) hashAddressedFile

        -- Since the store is content-addressed, we assume that two files with the same
        -- name have the same contents. Therefore, if a file already exists at this path,
        -- there is no reason to take any action.
        AlreadyPresent -> pure ()

    pure WriteResult{ hashAddressedFile, writeType }

runStream :: forall abort commit. HashFunction -> Handle
    -> Pipes.Producer Strict.ByteString (ExceptT abort IO) commit
    -> IO (Either abort (HashName, commit))
runStream hash handle stream =
    case writeAndHash hash handle of
        Fold.EffectfulFold{ Fold.initial, Fold.step, Fold.extract } ->
            Except.runExceptT $
                Pipes.foldM' step initial extract stream

writeAndHash :: HashFunction -> Handle
    -> Fold.EffectfulFold (ExceptT abort IO) Strict.ByteString HashName
writeAndHash (HashFunction hash) handle =
    (Fold.effect \chunk -> liftIO (Strict.ByteString.hPut handle chunk))
    *> (HashName <$> Fold.fold hash)

{-| Write a stream of strict ByteStrings to a hash-addressed directory

If the producer throws an exception, nothing will be written and the
exception will be re-thrown.

This is a simplified variant of 'writeExcept'. -}
writeStream :: forall commit m. MonadIO m =>
    Directory -- ^ Where to write
    -> Pipes.Producer Strict.ByteString IO commit -- ^ What to write
    -> m (commit, WriteResult)
writeStream dir source = voidExcept $ writeExcept dir $ Pipes.hoist lift source

voidLeft :: Either Void a -> a
voidLeft = \case{ Either.Left x -> absurd x; Either.Right x -> x }

voidExcept :: Functor m => ExceptT Void m a -> m a
voidExcept = Except.runExceptT >>> fmap voidLeft

{-| Write a lazy ByteString to a hash-addressed directory

This is a simplified variant of 'writeStream'. -}
writeLazy :: forall m. MonadIO m =>
    Directory -- ^ Where to write
    -> Lazy.ByteString -- ^ What to write
    -> m WriteResult
writeLazy dir lbs = writeStream dir (lbsProducer lbs) <&> (\((), x) -> x)

lbsProducer :: Lazy.ByteString -> Pipes.Producer Strict.ByteString IO ()
lbsProducer = Lazy.toChunks >>> Pipes.each
