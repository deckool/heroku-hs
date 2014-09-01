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

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C

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
    action <- getsRequest $ getHeader "X-GitHub-Event"
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
          --liftIO $ G.raw
    liftIO $ print f

papam :: Snap()
papam = do
  x <- liftIO $ S.readFile "app.json"
  liftIO $ print x
  let z = C.pack $ S.unpack x
  liftIO $ print z
  let xxx = commitList <$> decode z
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
  writeBS x

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