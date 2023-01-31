module HashAddressed.Directory
  (
    {- * Type -} ContentAddressedDirectory, init,
    {- * Write operations -}
            writeLazy, writeStreaming, writeEither, writeExcept,
            WriteResult (..), WriteType (..),
  )
  where

import Essentials
import HashAddressed.HashFunction

import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State (StateT)
import Data.Either (Either)
import Prelude (FilePath, IO)
import System.FilePath ((</>))

import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as Monad
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans.Resource as Resource
import qualified Control.Monad.Trans.State as Monad
import qualified Crypto.Hash.SHA256 as Hash
import qualified Data.ByteString as Strict
import qualified Data.ByteString as Strict.ByteString
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Strict.ByteString.Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.Either as Either
import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified System.IO.Temp as Temporary

data ContentAddressedDirectory =
  ContentAddressedDirectory
    { directory :: FilePath
    }

data WriteResult =
  WriteResult
    { contentAddressedFile :: FilePath
        {- ^ The file path where the contents written by the action
             now reside. This path includes the store directory. -}
    , writeType :: WriteType
    }

data WriteType = AlreadyPresent | NewContent

{-| Specification of a content-addressed directory -}
init ::
    HashFunction {- ^ Which hash function to use -}
    -> FilePath {- ^ Directory where content-addressed files are stored -}
    -> ContentAddressedDirectory
init SHA_256 = ContentAddressedDirectory

writeExcept :: forall m bad good. (MonadIO m, MonadError bad m) =>
    ContentAddressedDirectory
        {- ^ The content-addressed file store to write to; see 'init' -}
    -> (forall m'. (MonadIO m', MonadError bad m') => (Strict.ByteString -> m' ()) -> m' good)
        {- ^ Monadic action which is allowed to emit 'Strict.ByteString's, do I/O,
             and abort via 'MonadError'. If the action aborts (either by 'MonadError'
             or by 'IO') then nothing will be committed to the store. -}
    -> m (good, WriteResult)
writeExcept dir stream =
  do
    {-  Where the system in general keeps its temporary files  -}
    temporaryRoot <- Monad.liftIO Temporary.getCanonicalTemporaryDirectory

    (Except.liftEither =<<) $ Monad.liftIO $ Resource.runResourceT @IO
      do
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

        badOrGood <- Monad.liftIO $ Except.runExceptT $ copyStream stream handle

        {-  Once we're done writing the file, we no longer need the handle.  -}
        Resource.release handleRelease {- (🍓) -}

        case badOrGood of
            Either.Left bad -> pure (Either.Left bad)
            Either.Right (good, hashState) -> conclude dir temporaryFile hashState good

copyStream ::
    (forall m'. (MonadIO m', MonadError bad m') => (Strict.ByteString -> m' ()) -> m' good)
    -> IO.Handle
    -> ExceptT bad IO (good, Hash.Ctx)
copyStream stream handle = Monad.runStateT (stream (writeChunk handle)) Hash.init

{-  Action that we pass ot the continuation to write a chunk. It does two things:
    write to the handle, and update the hash state. -}
writeChunk :: IO.Handle -> Strict.ByteString -> StateT Hash.Ctx (ExceptT bad IO) ()
writeChunk handle chunk = do
    Monad.liftIO $ Strict.ByteString.hPut handle chunk
    State.modify' \hashState -> Hash.update hashState chunk

conclude :: MonadIO m => ContentAddressedDirectory -> FilePath
    -> Hash.Ctx -> good -> m (Either bad (good, WriteResult))
conclude dir temporaryFile hashState good =
  do
    {-  The final location where the file will reside  -}
    let contentAddressedFile = directory dir </>
            Strict.ByteString.Char8.unpack
                (Base16.encode (Hash.finalize hashState))

    {-  Another file of the same name in the content-addressed directory
        might already exist.  -}
    writeType <- Monad.liftIO (Directory.doesPathExist contentAddressedFile)
          <&> \case{ True -> AlreadyPresent; False -> NewContent }

    case writeType of

        {-  In one atomic step, this action commits the file to the store
            and prevents it from being deleted by the directory cleanup
            action (🧹).  -}
        NewContent -> Monad.liftIO $
            Directory.renamePath temporaryFile contentAddressedFile

        {-  Since the store is content-addressed, we assume that two files
            with the same name have the same contents. Therefore, if a file
            already exists at this path, there is no reason to take any
            action.  -}
        AlreadyPresent -> pure ()

    pure $ Either.Right (good, WriteResult{ contentAddressedFile, writeType })

{-| Write a stream of strict byte strings to a content-addressed directory

This is a simplified variant of 'writeExcept'. -}
writeEither ::
    ContentAddressedDirectory
        {- ^ The content-addressed file store to write to; see 'init' -}
    -> (forall m. MonadIO m => (Strict.ByteString -> m ()) -> m (Either bad good))
        {- ^ Monadic action which is allowed to emit 'Strict.ByteString's
             and do I/O. The action should return 'Either.Right' once the content
             has been successfully written. If the action returns 'Either.Left' or
             throws an exception, then nothing will be committed to the store. -}
    -> IO (Either bad (good, WriteResult))
writeEither dir stream = Except.runExceptT $
    writeExcept dir \w ->
        Except.liftEither =<< stream w

{-| Write a stream of strict byte strings to a content-addressed directory

This is a simplified variant of 'writeEither'. -}
writeStreaming ::
    ContentAddressedDirectory
        {- ^ The content-addressed file store to write to; see 'init' -}
    -> (forall m. MonadIO m => (Strict.ByteString -> m ()) -> m ())
        {- ^ Monadic action which is allowed to emit 'Strict.ByteString's
             and do I/O. If this action throws an exception, nothing will
             be written to the store. -}
    -> IO WriteResult
writeStreaming dir stream = writeEither dir (fmap Either.Right . stream) <&> \case
    Either.Left x -> absurd x
    Either.Right ((), result) -> result

{-| Write a lazy byte string to a content-addressed directory

This is a simplified variant of 'writeStreaming'. -}
writeLazy ::
    ContentAddressedDirectory
        {- ^ The content-addressed file store to write to; see 'init' -}
    -> Lazy.ByteString
        {- ^ The content to write to the store -}
    -> IO WriteResult
        {- ^ The file path where the contents of the lazy byte string
             now reside. This path includes the store directory. -}
writeLazy dir lbs = writeStreaming dir \w ->
    traverse_ w (Lazy.ByteString.toChunks lbs)
