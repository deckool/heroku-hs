{-# LANGUAGE OverloadedStrings #-}

import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras
import System.Directory 


main = do 
    y <- getDirectoryContents "bgimgs"
    let filtered = filter (not . (`elem` ["..", "."])) y
    x <- runRVar (choice filtered) DevRandom
    print x
