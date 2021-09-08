{-# OPTIONS_GHC -Wall #-}

module Sql where

import Parser
  ( Parser
  , pExit
  , pSpaces
  , choice
  )

import Json
  ( JsonValue(..)
  )

data SqlRequest = SqlExit
                | SqlSelectFrom

pSqlRequest :: Parser SqlRequest
pSqlRequest = do
  _ <- pSpaces
  choice "sql request"
    [ SqlExit <$ pExit
    ]

isHdrValid :: String -> (String, JsonValue) -> Bool
isHdrValid h1 ((JsonString h2)) = h1 == h2
isHdrValid _  _ = False

isConValid :: String -> 


-- | Traverse the input Json structure to verify scheme
isDbValid :: JsonValue -> Bool
isDbValid (JsonObject header:content:[]) =
  isHdrValid "database" header &&
isDbValid _ = False

-- | Try to apply well-formed request and return new database
tryApplyReq :: SqlRequest -> JsonValue -> IO JsonValue
tryApplyReq SqlExit jv  = pure jv
tryApplyReq SqlSelectFrom jv = pure jv
