{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

main = do
    src <- readFile "kk"
    let t = decode "[\"1\",\"2\",\"3\"]" :: Maybe [String]
    print t