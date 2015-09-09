{-# LANGUAGE
    OverloadedStrings
    #-}

module JUnit.Parser where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import qualified Data.Text as T
import Data.Time.Clock
import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import System.FilePath


-- * Basic parser JUnit test suite results.


data TestSuiteResult = TestSuiteResult
    { suiteName     :: T.Text
    , suiteErrors   :: T.Text
    , suiteNumTests :: T.Text
    , suiteFailures :: T.Text
    , suiteTime     :: T.Text -- FIXME Data.Time?
    , suiteTimeStamp:: T.Text -- FIXME Data.Time?
    } deriving (Eq, Show)


-- |
parseFromFile :: FilePath -> IO [TestSuiteResult]
parseFromFile file = do
    doc <- readFile def file
    let cursor = fromDocument doc
    return $ testSuiteResults cursor
  where
    testSuiteResults cur = cur $// element "testsuite" >=>
        \c -> zipWith6 TestSuiteResult
            (attribute "name" c)
            (attribute "errors" c)
            (attribute "tests" c)
            (attribute "failures" c)
            (attribute "time" c)
            (attribute "timestamp" c)


