module HashAddressed.HashFunction
  (
    {- * Type -} HashFunction (..),
    {- * Examples -} sha256,
  )
  where

import Essentials

import Fold.Pure (Fold (..))
import System.IO (FilePath)

import qualified Crypto.Hash.SHA256 as Hash
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Strict.ByteString.Char8

newtype HashFunction = HashFunction (Fold Strict.ByteString FilePath)

sha256 :: HashFunction
sha256 = HashFunction Fold
  { initial = Hash.init
  , step = Hash.update
  , extract = Strict.ByteString.Char8.unpack . Base16.encode . Hash.finalize
  }
