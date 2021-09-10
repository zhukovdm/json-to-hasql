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
  ( JsonValue(..)
  , parseJson
  )

import Parser
  ( run
  )

import Request
  ( Request(..)
  , isDbValid
  , tryApplyReq
  , pReq
  )

-- | Top-level function
main :: IO ()
main = do
  dbNames <- getArgs
  rawText <- tryReadDb dbNames
  case rawText of
    (Left  err) -> report $ Just err
    (Right txt) -> do
      pj <- parseJson txt
      case pj of
        (Left err) -> report $ Just err
        (Right jv) -> do
          let v = isDbValid jv
          if v then do
            _ <- putStrLn "database is ready! proceed with requests."
            getReq jv
          else
            report $ Just "database format is not valid"

dbAddr :: String
dbAddr = "dbs/"

-- | Returns string describing invalid arguments
invalidArgs :: IO (Either String String)
invalidArgs = return $ Left "invalid arguments"

-- | Reads entire database file as raw text if possible, otherwise error.
tryReadDb :: [FilePath] -> IO (Either String String)
tryReadDb [] = invalidArgs
tryReadDb [dbName] =
  handle (\ (_ :: IOException) -> return $ Left ("not possible to read " <> dbName)) $ do
    Right <$> readFile (dbAddr <> dbName)
tryReadDb _ = invalidArgs -- more than one argument

-- | Get request and pass for processing
getReq :: JsonValue -> IO ()
getReq jv = do
  i <- getLine
  let r = run pReq i
  case r of
    (Left err) -> do
      _ <- report $ Just $ show err
      getReq jv
    (Right req) -> do
      case req of
        ReqQuit -> writeDb $ show jv
        _ -> do
          newjv <- tryApplyReq req jv
          getReq newjv

-- | Asks name, try to open and write, repeat if failure.
writeDb :: String -> IO ()
writeDb txt = do
  handle (\ (_ :: IOException) -> writeDb txt) $ do
    _      <- putStr "enter output db name: "
    _      <- hFlush stdout
    dbName <- getLine
    writeFile (dbAddr <> dbName) txt

-- | Report read or parser error, if arrises.
report :: Maybe String -> IO ()
report Nothing = pure ()
report (Just err) = do
  putStrLn err
