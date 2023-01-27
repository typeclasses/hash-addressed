module HashAddressed.HashFunction where

import Essentials

{-| Hash function supported by the @hash-addressed@ library

    Currently only SHA-256 is supported, but others
    may be added in the future.
-}
data HashFunction =
    SHA_256 -- ^ SHA-256
  deriving stock (Eq, Ord)
