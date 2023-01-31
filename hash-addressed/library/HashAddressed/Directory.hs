module HashAddressed.Directory
  (
    {- * Type -} Directory, init,
    {- * Write operations -}
            writeLazy, writeStream, writeExcept,
            WriteResult (..), WriteType (..),
  )
  where

import Essentials
import HashAddressed.HashFunction

import System.FilePath ((</>))
import Pipes ((>->))

import qualified Data.Either as Either
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans as Monad
import qualified Control.Monad.Trans.Resource as Resource
import qualified Control.Monad.State as State
import qualified Crypto.Hash.SHA256 as Hash
import qualified Data.ByteString as Strict
import qualified Data.ByteString as Strict.ByteString
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Strict.ByteString.Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified System.IO.Temp as Temporary

{-| Specification of a hash-addressed directory

See 'init'. -}
data Directory = Directory{ directory :: IO.FilePath }

data WriteResult = WriteResult
  { hashAddressedFile :: IO.FilePath
      {- ^ The file path where the contents written by the action
            now reside. This path includes the store directory. -}
  , writeType :: WriteType
  }

data WriteType = AlreadyPresent | NewContent

init ::
    HashFunction {- ^ Which hash function to use -}
    -> IO.FilePath {- ^ Directory where hash-addressed files are stored

    Note that the utilities in "HashAddressed.Directory" do not create this
    directory; ensure that it already exists before attempting to write. -}
    -> Directory
init SHA_256 = Directory

{-| Write a stream of strict byte strings to a hash-addressed directory,
    possibly aborting mid-stream with an error value instead -}
writeExcept :: forall abort commit m. (IO.MonadIO m, Except.MonadError abort m) =>
    Directory {- ^ The hash-addressed file store to write to; see 'init' -}
    -> Pipes.Producer Strict.ByteString (Except.ExceptT abort IO.IO) commit
        {- ^ Producer of strict byte string content to write

             If this throws an exception, either in 'IO' on in 'ExceptT',
             nothing will be written. -}
    -> m (commit, WriteResult)
writeExcept dir stream = runResourceEither do

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
    let temporaryFile = temporaryDirectory </> "hash-addressed-file"

    {-  Create the file and open a handle to write to it  -}
    (handleRelease, handle) <- Resource.allocate
        (IO.openBinaryFile temporaryFile IO.WriteMode)
        IO.hClose {- (🍓) -}

    {-  Run the continuation, doing two things at once with the byte string
        chunks it gives us: write the file, and update a hash context -}
    abortOrCommit <- runStream handle stream

    {-  Once we're done writing the file, we no longer need the handle.  -}
    Resource.release handleRelease {- (🍓) -}

    traverse (traverse (finalize dir temporaryFile)) abortOrCommit

finalize :: Pipes.MonadIO m => Directory -> IO.FilePath -> Hash.Ctx -> m WriteResult
finalize dir temporaryFile hashState = do

    {-  The final location where the file will reside  -}
    let hashAddressedFile = directory dir </> hashStateName hashState

    {-  Another file of the same name in the content-addressed directory
        might already exist.  -}
    writeType <- Monad.liftIO $
          Directory.doesPathExist hashAddressedFile
          <&> \case{ True -> AlreadyPresent; False -> NewContent }

    case writeType of

        {-  In one atomic step, this action commits the file to the store
            and prevents it from being deleted by the directory cleanup
            action (🧹).  -}
        NewContent -> Monad.liftIO $
            Directory.renamePath temporaryFile hashAddressedFile

        {-  Since the store is content-addressed, we assume that two files
            with the same name have the same contents. Therefore, if a file
            already exists at this path, there is no reason to take any
            action.  -}
        AlreadyPresent -> pure ()

    pure WriteResult{ hashAddressedFile, writeType }

hashStateName :: Hash.Ctx -> IO.FilePath
hashStateName = Strict.ByteString.Char8.unpack . Base16.encode . Hash.finalize

runStream :: IO.Handle
    -> Pipes.Producer Strict.ByteString (Except.ExceptT abort IO.IO) commit
    -> Resource.ResourceT IO.IO (Either.Either abort (commit, Hash.Ctx))
runStream handle stream =
    Except.runExceptT $ runHashState $ Pipes.runEffect $
        hoistStream stream >-> Pipes.mapM_ (writeAndHash handle)

runHashState :: State.StateT Hash.Ctx m a -> m (a, Hash.Ctx)
runHashState x = State.runStateT x Hash.init

hoistStream :: Pipes.Producer chunk (Except.ExceptT abort IO.IO) commit
    -> Pipes.Producer chunk (State.StateT Hash.Ctx
        (Except.ExceptT abort (Resource.ResourceT IO.IO))) commit
hoistStream = Pipes.hoist (Monad.lift . Except.mapExceptT Monad.lift)

writeAndHash :: (IO.MonadIO m, State.MonadState Hash.Ctx m) =>
    IO.Handle -> Strict.ByteString -> m ()
writeAndHash handle chunk = do

    -- Write to the file
    Monad.liftIO $ Strict.ByteString.hPut handle chunk

    -- Update the state of the hash function
    State.modify' \hashState -> Hash.update hashState chunk

runResourceEither :: (IO.MonadIO m, Except.MonadError abort m) =>
    Resource.ResourceT IO.IO (Either.Either abort a) -> m a
runResourceEither =
    liftExceptIO . Except.ExceptT . Resource.runResourceT @IO.IO

liftExceptIO :: forall e m a. (IO.MonadIO m, Except.MonadError e m) =>
    Except.ExceptT e IO.IO a -> m a
liftExceptIO x = do
    r <- Except.runExceptT (Except.mapExceptT Monad.liftIO x)
    Except.liftEither r

{-| Write a stream of strict byte strings to a hash-addressed directory

This is a simplified variant of 'writeExcept'. -}
writeStream :: forall commit m. IO.MonadIO m =>
    Directory {- ^ The hash-addressed file store to write to; see 'init' -}
    -> Pipes.Producer Strict.ByteString IO.IO commit
          {- ^ Producer of strict byte string content to write

               If this throws an exception, nothing will be written. -}
    -> m (commit, WriteResult)
writeStream dir source = voidExcept $ writeExcept dir (Pipes.hoist Monad.lift source)

voidLeft :: Either.Either Void a -> a
voidLeft = \case{ Either.Left x -> absurd x; Either.Right x -> x }

voidExcept :: Functor m => Except.ExceptT Void m a -> m a
voidExcept = Except.runExceptT >>> fmap voidLeft

{-| Write a lazy byte string to a hash-addressed directory

This is a simplified variant of 'writeStream'. -}
writeLazy :: forall m. IO.MonadIO m =>
    Directory {- ^ The hash-addressed file store to write to; see 'init' -}
    -> Lazy.ByteString {- ^ Lazy byte string content to write -}
    -> m WriteResult
writeLazy dir lbs = writeStream dir (lbsProducer lbs) <&> (\((), x) -> x)

lbsProducer :: Lazy.ByteString -> Pipes.Producer Strict.ByteString IO.IO ()
lbsProducer = Lazy.toChunks >>> Pipes.each
