{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified FullBG.Git as G
import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as S
import           Control.Monad.IO.Class
import           Control.Monad
import           System.Process

import           System.Directory

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B


import           OpenSSL               (withOpenSSL)
import           Network.Http.Client

import qualified Network.HTTP.Conduit as N
import Network.HTTP.Headers
import Control.Monad.Trans.Resource

import           Network.HTTP.Headers

import qualified FileContent as FC

main :: IO ()
main = do
    httpServe (setPort 8000 config) site
        where
         config =
             setErrorLog  ConfigNoLog $
             setAccessLog ConfigNoLog $
             defaultConfig

site :: Snap ()
site =
    ifTop xxx <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          , ("/lol/", serveDirectory "../app")
          , ("/get", method Snap.Core.POST exist)
          , ("/set",  papam)
          , ("/am", tadam)
          --, ("zein", zain)
          , ("git", serveDirectory "FullBG")
          --, ("/js", serveDirectory "../semantic.gs")
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

xxx :: Snap ()
xxx = do
   modifyResponse $ addHeader "Content-Type" "application/json; charset=UTF-8"
   modifyResponse $ addHeader "Server" "One"
   writeBS "{\"message\":\"hello world\",\"route1\":\"/git will get you to a Github wrapper configured with webhooks\"}"

exist :: Snap ()
exist = do
    action <- getsRequest $ Snap.Core.getHeader "X-GitHub-Event"
    acti_on <- maybe pass return action
    fooParam <- getsRequest $ rqParam "payload"
    told <- maybe pass return fooParam
    --test <- liftIO $ system "cd ~/dinx60/home/github/angular_views/src/; ./bin.sh"
    --liftIO $ print test
    reqPath <- fmap rqPathInfo getRequest
--    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    let no = S.concat told
    y <- liftIO $ getDirectoryContents "."
    let y1 = concat y
    liftIO $ print y1
    liftIO $ S.writeFile "app.json" no
    let z = C.pack $ S.unpack no
    let f = commitList <$> decode z
    case f of
      Nothing -> do
          liftIO $ G.raw
      Just ps -> do
          let a = fmap author ps
          let b = fmap username a
          liftIO $ print b
          --liftIO $ print a
          liftIO $ G.raw
    liftIO $ print f

check = do
    let args = "https://api.github.com/repos/deckool/heroku-hs/collaborators"
    case N.parseUrl args of
        Nothing -> print "Sorry, invalid URL"
        Just req -> runResourceT $ N.withManager $ \manager -> do
            let custom_header = ("user-agent", "x")
            let reqHead = req { N.requestHeaders = [custom_header] }
            res <- N.httpLbs reqHead manager
            let jsonresponse = N.responseBody res
            liftIO $ B.writeFile "zzz.json" jsonresponse

tadam :: Snap()
tadam = do
<<<<<<< HEAD
  x <- liftIO $ B.readFile "zzz.json"
  writeLBS x
=======
    xxy <- liftIO $ FC.main "src/doi.hs"
    let u = S.pack xxy
    liftIO $ print u
    writeBS u

>>>>>>> 110f2f35b2564dd4fcfe824402ee84d09d90e9f3

papam :: Snap()
papam = do
  x <- liftIO $ B.readFile "app.json"
  liftIO $ print x
  --let z = C.pack $ S.unpack x
  --liftIO $ print z
  let xxx = commitList <$> decode x
  liftIO $ print xxx
  case xxx of
    Nothing -> liftIO $ print "Nothing"
    Just ps -> do
        liftIO $ print ps
        let a = fmap author ps
        let b = fmap username a
        let c = concatMap modified ps
        --case c of
        --  [Just x] -> liftIO $ print x
        liftIO $ print b
        --let z =  take 1 c
        liftIO $ print $ filter (`elem` [""]) c
        --liftIO $ print a
        liftIO $ G.raw
  writeLBS x

data Pula = Pula {after :: String} deriving (Show)

instance FromJSON Pula where
    parseJSON (Object o) = Pula <$> o .: "after"
    parseJSON _ = mzero

newtype CommitList = CommitList {commitList :: [Commit]}

instance FromJSON CommitList where
    parseJSON (Object o) = CommitList <$> o .: "commits"
    parseJSON _ = mzero

data Commit = Commit {ids :: String, message :: String, url :: String, modified :: [String], author :: Auth} deriving (Show)

instance FromJSON Commit where
    parseJSON (Object o) = Commit <$> o .: "id" <*> o .: "message" <*> o .: "url" <*> o .: "modified" <*> o .: "author"
    parseJSON _ = mzero

data Auth = Auth {name :: String, email :: String, username :: String} deriving (Show)

instance FromJSON Auth where
    parseJSON (Object o) = Auth <$> o .: "name" <*> o .: "email" <*> o .: "username"
    parseJSON _ = mzero

{--zain :: Snap()
zain = do
  let jso_n = "{\"total\":1,\"movies\":[ {\"id\":\"771315522\",\"zd\":\"771315522\",\"title\":\"Harry Potter and the Philosophers Stone (Wizard's Collection)\",\"posters\":{\"thumbnail\":\"http://content7.flixster.com/movie/11/16/66/11166609_mob.jpg\",\"profile\":\"http://content7.flixster.com/movie/11/16/66/11166609_pro.jpg\",\"detailed\":\"http://content7.flixster.com/movie/11/16/66/11166609_det.jpg\",\"original\":\"http://content7.flixster.com/movie/11/16/66/11166609_ori.jpg\"}}]}"
  let xxx = movieList <$> decode jso_n
  liftIO $ print xxx
  case xxx of
    Nothing -> liftIO $ print "Nothing"
    Just ps -> do
        let a = fmap zd ps
        liftIO $ print a
  writeLBS jso_n--}