{-# LANGUAGE OverloadedStrings #-}
module Main where

import            Control.Applicative
import            Snap.Core
import            Snap.Util.FileServe
import            Snap.Http.Server
import            Text.Blaze.Html5 hiding (param)
import qualified  Text.Blaze.Html5 as H
import qualified  Text.Blaze.Html5.Attributes as A
import qualified  Text.Blaze.Html.Renderer.Pretty as P
import qualified  Data.ByteString.Char8 as S
import            Database.Redis
import            Control.Monad.IO.Class
import            Data.Either.Utils
import            Data.Maybe

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
--    ifTop (writeBS "hello digital ocean") <|>
    ifTop noHandler <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    theParam <- maybe pass return param
    a <- liftIO $ what theParam
    writeBS a

what name = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        existHash <- exists name
        if existHash == Right True
            then do
              h <- get name
              let i = getValue h
              return i
            else return "no no no"

user :: String -> S.ByteString
user x = S.pack $ "user:" ++ x

getValue :: Either a1 (Maybe a) -> a
getValue x = fromJust $ fromRight x

noHandler :: Snap ()
noHandler = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    let clicky = "<script src=\"http://static.getclicky.com/js\" type=\"text/javascript\"></script><script type=\"text/javascript\">try{clicky.init(100687310);}catch(e){}</script><noscript><p><img alt=\"Clicky\" width=\"1\" height=\"1\" src=\"//in.getclicky.com/100687310ns.gif\"/></p></noscript>"
    writeBS $ S.pack $ P.renderHtml (existing clicky)

existing :: String -> Html
existing clicky = docTypeHtml $ do
    H.head $ do
        H.link H.! A.rel "stylesheet" H.! A.href "http://fonts.googleapis.com/css?family=Megrim" H.! A.type_ "text/css"
        H.link ! A.rel "stylesheet" ! A.href "http://www.responsivegridsystem.com/downloads/css/col.css" ! A.type_ "text/css"    
        H.link ! A.rel "stylesheet" ! A.href "http://www.responsivegridsystem.com/downloads/css/2cols.css" ! A.type_ "text/css" 
        H.title $ "Hello Digital Ocean from haskell!"
    H.body ! A.style "font-family:Megrim,cursive;text-align:center;" $ do
        H.h1 $ "Hello digitalOcean"
        H.h1 ! A.style "font-size:80px;text-shadow:1px 1px 1px #000;margin-bottom:0;margin-top:0;" $ "Welcome to data porn"
        H.img ! A.src "http://i.imgur.com/kya8wjf.gif"
        H.div ! A.class_ "section group" $ do
           H.div ! A.style "border-radius:10px;background:black;color:white;" ! A.class_ "col span_1_of_2" $ do
                H.h1 "designer"
           H.div ! A.style "border-radius:10px;background:#1abc9c;color:white;" ! A.class_ "col span_1_of_2" $ do 
                H.h1 "programmer"
        H.img ! A.src "https://www.digitalocean.com/assets/v2/badges/digitalocean-badge-black.png" 
        (preEscapedToHtml clicky)
