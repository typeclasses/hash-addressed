module HashAddressed.Directory
  (
    {- * Type -} ContentAddressedDirectory, init,
    {- * Write operations -} writeStreaming, writeLazy,
            WriteResult (..), WriteType (..),
  )
  where

import Essentials
import HashAddressed.HashFunction

import Control.Monad.IO.Class (MonadIO)
import Data.Function (flip)
import Prelude (FilePath, IO)
import System.FilePath ((</>))

import qualified Control.Monad.Trans.Class as Monad
import qualified Control.Monad.Trans.Resource as Resource
import qualified Control.Monad.Trans.State as Monad
import qualified Control.Monad.Trans.State as State
import qualified Crypto.Hash.SHA256 as Hash
import qualified Data.ByteString as Strict
import qualified Data.ByteString as Strict.ByteString
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Strict.ByteString.Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy as Lazy.ByteString
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

{-| Write a stream of strict byte strings to a content-addressed directory -}
writeStreaming ::
    ContentAddressedDirectory
        {- ^ The content-addressed file store to write to; see 'init' -}
    -> (forall m. MonadIO m => (Strict.ByteString -> m ()) -> m ())
        {- ^ Monadic action which is allowed to emit 'Strict.ByteString's
             and do I/O -}
    -> IO WriteResult
writeStreaming dir continue = Resource.runResourceT @IO
  do
    {-  Where the system in general keeps its temporary files  -}
    temporaryRoot <- Monad.lift Temporary.getCanonicalTemporaryDirectory

    {-  We do not yet know what the final file path will be, because that is
        determined by the hash of the contents, which we have not computed yet. -}

    {-  We will write the file into this directory and then move it out in an
        atomic rename operation that will commit the file to the store.  -}
    (_, temporaryDirectory) <- Resource.allocate
        (Temporary.createTempDirectory temporaryRoot "cafs")
        Directory.removeDirectoryRecursive {- (🧹) -}

    {-  If the file never gets moved, then when the directory is removed
        recursively (🧹), the file will be destroyed along with it.

        If the file does get moved, the directory will be destroyed (🧹),
        but the file, which no longer resides within the directory, will remain. -}

    {-  The path of the file we're writing, in its temporary location  -}
    let temporaryFile = temporaryDirectory </> "cafs-file"

    {-  Create the file and open a handle to write to it  -}
    (handleRelease, handle) <- Resource.allocate
        (IO.openBinaryFile temporaryFile IO.WriteMode)
        IO.hClose {- (🍓) -}

    {-  Run the continuation, doing two things at once with the byte string
        chunks it gives us:  -}
    hashState :: Hash.Ctx <- Monad.lift $ flip Monad.execStateT Hash.init $
        continue \chunk ->
          do
            {-  1. Write to the file  -}
            Monad.lift $ Strict.ByteString.hPut handle chunk

            {-  2. Update the state of the hash function  -}
            State.modify' \hashState -> Hash.update hashState chunk

    {-  Once we're done writing the file, we no longer need the handle.  -}
    Resource.release handleRelease {- (🍓) -}

    {-  The final location where the file will reside  -}
    let contentAddressedFile = directory dir </>
            Strict.ByteString.Char8.unpack
                (Base16.encode (Hash.finalize hashState))

    {-  Another file of the same name in the content-addressed directory
        might already exist.  -}
    writeType <- Monad.lift (Directory.doesPathExist contentAddressedFile)
          <&> \case{ True -> AlreadyPresent; False -> NewContent }

    case writeType of

        {-  In one atomic step, this action commits the file to the store
            and prevents it from being deleted by the directory cleanup
            action (🧹).  -}
        NewContent -> Monad.lift $
            Directory.renamePath temporaryFile contentAddressedFile

        {-  Since the store is content-addressed, we assume that two files
            with the same name have the same contents. Therefore, if a file
            already exists at this path, there is no reason to take any
            action.  -}
        AlreadyPresent -> pure ()

    pure WriteResult{ contentAddressedFile, writeType }

{-| Write a lazy byte string to a content-addressed directory -}
writeLazy ::
    ContentAddressedDirectory
        {- ^ The content-addressed file store to write to; see 'init' -}
    -> Lazy.ByteString
        {- ^ The content to write to the store -}
    -> IO WriteResult
        {- ^ The file path where the contents of the lazy byte string
             now reside. This path includes the store directory. -}
writeLazy dir lbs = writeStreaming dir \writeChunk ->
    traverse_ writeChunk (Lazy.ByteString.toChunks lbs)
