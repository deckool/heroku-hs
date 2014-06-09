{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as S
import           Control.Monad.IO.Class
import           System.Process
import           Snap.Extras.JSON

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
   writeBS "{\"message\":\"hello world\",\"message1\":\"What's up world?\"}"
   r <- fmap rspStatusReason getResponse
   writeBS r
   req <- fmap rqServerName getRequest
   writeBS req

exist :: Snap ()
exist = do
    fooParam <- getsRequest $ rqParam "payload"
    told <- maybe pass return fooParam
    test <- liftIO $ system "cd ~/dinx60/home/github/angular_views/src/; ./bin.sh"
    reqPath <- fmap rqPathInfo getRequest
--    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    let no = S.concat told
    let s = "{blah: blahbla}"
    liftIO $ S.writeFile "app.json" no
    writeBS no

papam :: Snap()
papam = do
  x <- liftIO $ S.readFile "app.json"
  writeBS x