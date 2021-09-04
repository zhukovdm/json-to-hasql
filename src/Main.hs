{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
 ( IOException
 , handle
 )

import System.IO
  ( hFlush
  , stdout
  )

-- import Json
--   ( --parseJson
--   , --writeJson
--   )

-- import Sql
--   ( --modifyJson
--   )

type ReadError = String

type ReportedError = String

type RawText = String

-- | Top-level function
main :: IO ()
main = do
  dbName  <- getDbName
  rawText <- tryReadDb dbName
  case rawText of
    (Left  err) -> report (Just err)
    (Right txt) -> writeDb txt

-- | Gets database name from the user via shell.
getDbName :: IO FilePath
getDbName = do
  _ <- putStr "enter input db name: "
  _ <- hFlush stdout
  getLine

-- | Reads entire database file as raw text if possible, otherwise error.
tryReadDb :: FilePath -> IO (Either ReadError RawText)
tryReadDb dbName =
  handle (\ (_ :: IOException) -> return $ Left ("not possible to read " <> dbName)) $ do
    Right <$> readFile dbName

-- | Asks repeatedly db name, try to open and write. Repeat in case of failure.
writeDb :: RawText -> IO ()
writeDb txt = do
  handle (\ (_ :: IOException) -> writeDb txt) $ do
    _      <- putStr "enter output db name: "
    _      <- hFlush stdout
    dbName <- getLine
    writeFile dbName txt

-- | Report read or parser error, if arrises.
report :: Maybe ReportedError -> IO ()
report Nothing = pure ()
report (Just err) = do
  putStrLn err
