{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    let args = "https://api.github.com/repos/deckool/heroku-hs/collaborators"
    case parseUrl args of
        Nothing -> putStrLn "Sorry, invalid URL"
        Just req -> withManager $ \manager -> do
            let custom_header = ("user-agent", "x")
            let reqHead = req { requestHeaders = [custom_header] }
            res <- httpLbs reqHead manager
            liftIO $ do
            print $ responseStatus res
            mapM_ print $ responseHeaders res
            print $ responseBody res
                        