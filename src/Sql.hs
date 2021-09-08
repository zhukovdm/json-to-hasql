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

isSqlValidScheme :: [(String, JsonValue)] -> Bool
isSqlValidScheme _ = True

isSqlValidContent :: [(String, JsonValue)] -> Bool
isSqlValidContent _ = True

validateJson :: JsonValue -> Bool
validateJson (JsonObject o) = isSqlValidScheme o && isSqlValidContent o
validateJson _ = False

-- | Try to apply well-formed request and return new database
tryApplyReq :: SqlRequest -> JsonValue -> IO JsonValue
tryApplyReq SqlExit jv  = pure jv
tryApplyReq SqlSelectFrom jv = pure jv
