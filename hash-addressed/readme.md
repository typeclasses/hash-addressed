`hash-addressed` is a simple system for maintaining a directory wherein each
file's name is a hash of its content.

```haskell
import HashAddressed.Directory
import HashAddressed.HashFunction
```

```haskell
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Pipes
```

### `Directory`

First define a `Directory` value by specifying which hash function to use and
the path of the directory in which the files shall be kept.

```haskell
data Directory = Directory { directoryPath :: FilePath,
                             hashFunction :: HashFunction }
```

Presently the only supported hash function is `sha256`.

Ensure that `directoryPath` is the path of an existing directory. You can then
write files into the directory using one of three *write* functions:
`writeLazy`, `writeStream`, and `writeExcept`.

### `writeLazy`

`writeLazy` is the simplest to use; just give it a lazy `ByteString`.

```haskell
writeLazy :: forall m. MonadIO m =>
    Directory -> Lazy.ByteString -> m WriteResult
```

```haskell
data WriteResult = WriteResult{ hashAddressedFile :: FilePath,
                                writeType :: WriteType }
```

```haskell
data WriteType = AlreadyPresent | NewContent
```

`WriteResult` gives you the path of the file in the store, including the path of
the store itself. Because a hash-addressed store can never contain duplicate
files, writing a file has no effect if the content is already present; the
`WriteType` value indicates whether the file was actually written by this action
or was present in the store already.

### `writeStream`

The limitation of `writeLazy` is that it doesn't allow streaming. Thus enters
`writeStream`, which uses a pipes `Producer` to represent the content. The
producer can perform `IO` while generating stream content (for example, perhaps
it reads byte strings from a network socket). The producer can also return a
value (the `commit` type parameter) that will be returned alongside the
`WriteResult`.

```haskell
writeStream :: forall commit m. MonadIO m =>
    Directory
    -> Pipes.Producer Strict.ByteString IO commit
    -> m (commit, WriteResult)
```

All operations that write into a hash-addressed `Directory` are performed by
first writing the content somewhere within the system temporary directory and
then moving the file to its target location. This ensures that the store never
makes visible the results of a partial write. If the producer throws an
exception, everything written so far will be deleted and no content will be
written to the `Directory`.

### `writeExcept`

If there is some interesting way in which your stream can fail, you may wish to
use `writeExcept` instead. In this variant, the producer returns an
`Either abort commit` indicating whether the result should be committed to the
store. Return a `Left` result to signal that an error has occurred. The
`writeExcept` action will then throw the `abort` value into a `MonadError`
context.

```haskell
writeExcept :: forall abort commit m. (MonadIO m, MonadError abort m) =>
    Directory -> Pipes.Producer Strict.ByteString IO (Either abort commit)
    -> m (commit, WriteResult)
```
