{-# LANGUAGE
    OverloadedStrings
    #-}
module Lib where

import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T


parseFromFile :: String -> IO ()
parseFromFile file = do
    doc <- readFile def file
    let cursor = fromDocument doc
    print (failures cursor, tests cursor)
  where
    failures c = c $// element "testsuite" >=> attribute "failures"
    tests c    = c $// element "testsuite" >=> attribute "tests"
