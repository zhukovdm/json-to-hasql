{-# OPTIONS_GHC -Wall #-}

module Request where

import Data.Char
  ( isLetter
  )

import Parser
  ( Parser
  , many1
  , matchToken
  , pExit
  , pSpaces
  , satisfy
  , sepBy
  , choice
  )

import Json
  ( JsonValue(..)
  )

data Request = ReqExit
             | ReqSelectFrom ([String],String)
             | ReqFindWith (String, JsonValue)
             | ReqSetToIn

pReqSelectFrom :: Parser ([String], String)
pReqSelectFrom = do
  _ <- matchToken "select"
  c <- many1 il `sepBy` matchToken ","
  _ <- matchToken "from"
  t <- many1 il
  return (c, t)
    where
      il = satisfy "is letter" isLetter

pReq :: Parser Request
pReq = do
  _ <- pSpaces
  choice "request"
    [ ReqExit <$ pExit
    , ReqSelectFrom <$> pReqSelectFrom
    ]

-- | Verifies if json value is a json string
isValidJsStr :: JsonValue -> Bool
isValidJsStr (JsonString js) = foldr ((&&).isLetter) True js
isValidJsStr _               = False

-- | Verify only length of a row, no explicit types
isRowValid :: Int -> JsonValue -> Bool
isRowValid n (JsonArray xs) = n == length xs
isRowValid _ _              = False

-- | Verifies if table is valid
isTableValid :: (String, JsonValue) -> Bool
isTableValid (_:_, JsonArray ((JsonArray cols):xs)) =
  foldr ((&&).isValidJsStr) True cols &&
  foldr ((&&).isRowValid (length cols)) True xs
isTableValid _ = False

-- | Traverse the input Json structure to verify scheme
isDbValid :: JsonValue -> Bool
isDbValid (JsonObject ts) = foldr ((&&).isTableValid) True ts
isDbValid _ = False

-- | Request column search
-- trySelectFrom :: [String] -> String -> JsonValue


-- | Request row search
-- tryFindWith :: 

-- | Try to apply well-formed request and return new database
tryApplyReq :: Request -> JsonValue -> IO JsonValue
tryApplyReq ReqExit t = pure t
tryApplyReq (ReqSelectFrom (cs, t)) jv = do
  return jv
