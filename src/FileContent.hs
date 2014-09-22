{-# LANGUAGE OverloadedStrings #-}

module FileContent where

import           Network.HTTP.Conduit hiding (path)
import qualified Data.ByteString.Lazy as L
import           Data.Aeson
import           Control.Applicative
import           Control.Monad
import qualified Codec.Binary.Base64.String as B64

main a = do
  let uri = "https://api.github.com/repos/deckool/heroku-hs/contents/" ++ a ++ "?ref=no_github_readme"
  initReq <- parseUrl uri
  let custom_header = ("User-agent", "cat")
  let req' = initReq { secure = True,requestHeaders = [custom_header] } -- Turn on https
{--  let req = (flip urlEncodedBody) req' $
             [ ("longUrl", "http://www.google.com/")
  --           ,
             ]--}
  print req'
  response <- withManager $ httpLbs req'
  path_ response


path_ a = do
  let yyy = responseBody a
  let xxx = path <$> decode yyy
  case xxx of
    Just z -> do
    	--writeFile "decoded.json" $ B64.decode z
    	return z
    Nothing -> return "nada"

{--f a = do
  let links0 = _links <$> decode yyy
  case links0 of
    Just x -> print x
    Nothing -> print "nada"

  let self0 = self <$> links0
  case self0 of
    Just x -> print x
    Nothing -> print "nada"--}

data Content = Content { content :: String, size :: Int, path :: String, _links :: Links} deriving (Show)

instance FromJSON Content where
    parseJSON (Object o) = Content <$> o .: "content" <*> o .: "size" <*> o .: "path" <*> o .: "_links" 
    parseJSON _ = mzero

data Links = Links {self :: String, git :: String, html :: String} deriving (Show)

instance FromJSON Links where
    parseJSON (Object o) = Links <$> o .: "self" <*> o .: "git" <*> o .: "html"
    parseJSON _ = mzero