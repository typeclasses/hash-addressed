0.2.0.0 (2023-02-08)
----------------------------------------------------------------

Change type of stream parameter in `writeExcept` from

```haskell
Producer ByteString (ExceptT abort IO) commit
```

to

``haskell
Producer ByteString IO (Either abort commit)
```

The new version is equivalent (via the `ExceptT` constructor) to

``haskell
ExceptT abort (Producer ByteString IO) commit
```

and so what this change is doing is reversing the order of the
monad transformers. The overall result is the same, but the new
version seems slightly easier to work with.


0.1.0.0 (2023-01-31)
----------------------------------------------------------------

### HashFunction

`HashFunction` type is no longer opaque; any hash function can be supported.
The type is now a newtype for `Fold` from the `gambler` library.

### Directory

Renamed `ContentAddressedDirectory` to `Directory`

`Directory` constructor is now exported

Removed `init` function, which is redundant to `Directory` constructor

### Pipes

Now using the `pipes` library to express streams.

Removed:

```haskell
writeStreaming :: ContentAddressedDirectory
    -> (forall m. MonadIO m => (ByteString -> m ()) -> m ())
    -> IO WriteResult
```

Added:

```haskell
writeStream :: MonadIO m =>
    Directory -> Producer ByteString IO a -> m (a, WriteResult)
```

Removed:

```haskell
writeEither :: ContentAddressedDirectory
    -> (forall m. MonadIO m => (ByteString -> m ()) -> m (Either bad good))
    -> IO (Either bad (good, WriteResult))
```

Added:

```haskell
writeExcept :: (MonadIO m, MonadError abort m) =>
    Directory
    -> Producer ByteString (ExceptT abort IO) commit
    -> m (commit, WriteResult)
```

### WriteResult

Renamed field from `contentAddressedFile` to `hashAddressedFile`


0.0.1.0 (2023-01-27)
----------------------------------------------------------------

Add `HashAddressed.Directory.writeEither`


0.0.0.0 (2023-01-27)
----------------------------------------------------------------

Initial release
