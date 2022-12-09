module Lib (readFile') where

import qualified Data.ByteString as B (readFile)
import Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)

readFile' :: FilePath -> IO Text
readFile' f = T.decodeUtf8 <$> B.readFile f
