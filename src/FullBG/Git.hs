{-# LANGUAGE OverloadedStrings #-}

module FullBG.Git where

import           Prelude hiding (head, id, div)
import           Text.Blaze.Html5 hiding (map)
import           Text.Blaze.Html5.Attributes hiding (title,form)
import qualified Text.Blaze.Html.Renderer.Pretty as P
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as D
import           System.Directory
--import           Data.Random
--import           Data.Random.Source.DevRandom
--import           Data.Random.Extras
import           Control.Monad
import qualified Clay as Cl
import           Snap.Blaze.Clay
import qualified Github.Issues.Comments as Github
import           Data.List (intercalate)
import           Text.Discount
import qualified Github.GitData.Blobs as Github
import qualified Github.GitData.Readme as Github
import           Codec.Binary.Base64.String
import           Control.Monad.IO.Class
--import qualified Network.HTTP.Conduit as N



draw :: String -> String -> String -> String-> String -> String -> Html
draw string sndString comments z y click = docTypeHtml $ do
    head $ do
        title "fullBG"
        link ! rel "shortcut icon" ! href "favicon.png"
        meta ! httpEquiv "Content-Type" ! content "text/html;charset=UTF-8"
        Text.Blaze.Html5.style ! rel "stylesheet" ! type_ "text/css" $ clayPretty herStylesheet
        script ! type_ "text/javascript" $ "function toggle_visibility(b){var a=document.getElementById(b);if(a.style.display==\"none\"){a.style.display=\"block\"}else{a.style.display=\"none\"}};"
        link ! rel "stylesheet" ! href "http://fonts.googleapis.com/css?family=Ubuntu:300,400,500,700,300italic,400italic,500italic,700italic" ! type_ "text/css"
--        link ! rel "stylesheet" ! type_ "text/css" ! href "style.css"
        Text.Blaze.Html5.style ! media "screen" ! type_ "text/css" $ (toHtml string)
    body ! id "content" $ do
        form ! action "fullBG" ! method "POST" ! autocomplete "off" $ do
            button ! type_ "button" ! Text.Blaze.Html5.Attributes.style "background:#e74c3c !important;" ! onclick "toggle_visibility('form');" $ "show the void"
        form ! id "form" ! action "fullBG" ! method "POST" ! autocomplete "off" $ do        
            input ! type_ "text" ! placeholder "write" ! name "say" ! value "" ! required "" ! maxlength "4"
            input ! type_ "text" ! placeholder "here" ! name "say" ! value ""  ! maxlength "4"
            input ! type_ "text" ! placeholder "there" ! name "say" ! value ""  ! maxlength "4"
            input ! type_ "text" ! placeholder "everywhere" ! name "say" ! value ""  ! maxlength "4"
            button ! type_ "submit" ! class_ "formB" $ "enter the void"
            button ! type_ "button" ! Text.Blaze.Html5.Attributes.style "background:#e74c3c !important;" ! onclick "toggle_visibility('form');" $ "hide the void"
            p ! Text.Blaze.Html5.Attributes.style "font-weight:normal;color:white;margin:0;font-family: \"ocraregular\", \"OCR A Std\", \"OCR A\", Courier, \"Courier New\", monospace;" $ "1031 5054 9821 2220 and 5579 7283 6544 2325 are free for testing"
        article ! dataAttribute "provider" "Deck Pope" ! class_ "article" $ do
            (preEscapedToHtml $ D.unpack $ parseMarkdown compatOptions $ D.pack z)
            "If you want to improve this post send me"
            a ! href "https://github.com/deckool/heroku-hs/edit/master/README.md" $ "a pull request."
            "You can comment using"
            a ! href (toValue y) $ "Github Issues"
            "You will need an account."
            (preEscapedToHtml comments)
        div ! id "footer" $ do 
            h5 $ "Made using Sexy and <3"
        script ! type_ "text/javascript" $ (toHtml sndString)
        (preEscapedToHtml click)


same s i = do
    y <- getDirectoryContents "FullBG/bgimgs"
    let filtered = filter (not . (`elem` ["..", "."])) y
--    x <- runRVar (choice filtered) DevRandom
    let js = "var strings="++(show filtered)++";var randomIndex=Math.floor(Math.random()*strings.length);var randomString=strings[randomIndex];var c=\"bgimgs/\";var e=new Image();e.src=(c+randomString);document.body.appendChild(e);"
    css <- readFile "FullBG/style.css"
    let clicky = "<script src=\"http://static.getclicky.com/js\" type=\"text/javascript\"></script><script type=\"text/javascript\">try{clicky.init(100687310);}catch(e){}</script><noscript><p><img alt=\"Clicky\" width=\"1\" height=\"1\" src=\"//in.getclicky.com/100687310ns.gif\"/></p></noscript>"
-- Git comments
    possibleComments <- Github.comments "deckool" "heroku-hs" 1
    case possibleComments of
         (Left error) -> putStrLn $ "Error: " ++ show error
         (Right issues) -> do
             let issuesComments = intercalate "" $ Prelude.map formatComment issues
             C.writeFile "FullBG/index.html" $ C.pack $ P.renderHtml $ draw css js issuesComments s i clicky

--flame = do
--  possibleBlob <- Github.blob "deckool" "heroku-hs" "6cbf346f11b05efe4c889705e937fa0c6b01faa6"
--  case possibleBlob of
--    (Left error) -> putStrLn $ "Error: " ++ (show error)
--    (Right blob) -> putStrLn $ (decode $ Github.blobContent blob)
-------------------------------------------------------------------------
--------
-- the very fucked up part is that on every commit the sha will change so there are actually lots of strings to pull in
-- order to retrieve purely programmatically from git the wanted SHA. So using raw instead of api might work.
--------

raw = do
    possibleRead <- Github.readme "deckool" "heroku-hs"
--    print possibleBlob
    case possibleRead of
      (Left error) -> putStrLn $ "Error: " ++ (show error)
--      (Right blob) -> putStrLn $ decode $ Github.blobContent blob
      (Right x) -> do
        let y = decode $ Github.readmeContent x
        let issueNo = "https://github.com/deckool/heroku-hs/issues/" ++ (take 1 y)
        let blob = drop 1 y
        same blob issueNo
    putStrLn ":)"

formatComment comment =
    "<div class=\"comment\">" ++
    "<img class=\"avatar\" src=\"" ++ avatar ++"\"height=\"50\" width=\"50\">" ++
    "<p class=\"comm\">" ++ name ++
    " -> " ++ date ++ "</p>" ++
    (D.unpack (parseMarkdown compatOptions (D.pack comBody))) ++ "</div>"
    where
        avatar = Github.githubOwnerAvatarUrl $ Github.issueCommentUser comment
        name = Github.githubOwnerLogin $ Github.issueCommentUser comment
        date = show $ Github.fromGithubDate $ Github.issueCommentUpdatedAt comment
        comBody = Github.issueCommentBody comment

-- it seems her is hard to find , i guess i'll stick with him for some time :))
-------------------------------------------------------------------------------------------------

herStylesheet :: Cl.Css
herStylesheet = do
     Cl.article Cl.? do
         Cl.before Cl.& do
      	     Cl.backgroundImage (Cl.url "https://secure.gravatar.com/avatar/d9804a923910b29d5b59540e6d42ed6e?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png")
