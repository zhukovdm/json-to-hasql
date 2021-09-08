{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
 ( IOException
 , handle
 )

import System.Environment
  ( getArgs
  )

import System.IO
  ( hFlush
  , stdout
  )

import Json
  ( parseJson
  )

type ReadError = String

type ReportedError = String

type RawText = String

-- | Top-level function
main :: IO ()
main = do
  dbNames <- getArgs
  rawText <- tryReadDb dbNames
  case rawText of
    (Left  err) -> report (Just err)
    (Right txt) -> do
      json <- parseJson txt
      case json of
        (Left err) -> report (Just err)
        (Right jv) -> writeDb (show jv)

dbAddr :: String
dbAddr = "dbs/"

-- | Returns string describing invalid arguments
invalidArgs :: IO (Either ReadError RawText)
invalidArgs = return $ Left "invalid arguments"

-- | Reads entire database file as raw text if possible, otherwise error.
tryReadDb :: [FilePath] -> IO (Either ReadError RawText)
tryReadDb [] = invalidArgs
tryReadDb [dbName] =
  handle (\ (_ :: IOException) -> return $ Left ("not possible to read " <> dbName)) $ do
    Right <$> readFile (dbAddr <> dbName)
tryReadDb _ = invalidArgs -- more than one argument

-- | Asks name, try to open and write, repeat if failure.
writeDb :: RawText -> IO ()
writeDb txt = do
  handle (\ (_ :: IOException) -> writeDb txt) $ do
    _      <- putStr "enter output db name: "
    _      <- hFlush stdout
    dbName <- getLine
    writeFile (dbAddr <> dbName) txt

-- | Report read or parser error, if arrises.
report :: Maybe ReportedError -> IO ()
report Nothing = pure ()
report (Just err) = do
  putStrLn err
