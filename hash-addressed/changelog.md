0.1.0.0 (2023-01-31)
----------------------------------------------------------------

Renamed `ContentAddressedDirectory` to `Directory`

Renamed field from `contentAddressedFile` to `hashAddressedFile`

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


0.0.1.0 (2023-01-27)
----------------------------------------------------------------

Add `HashAddressed.Directory.writeEither`


0.0.0.0 (2023-01-27)
----------------------------------------------------------------

Initial release
