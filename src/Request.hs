{-# OPTIONS_GHC -Wall #-}

module Request where

import Parser
  ( Parser
  , pExit
  , pSpaces
  , choice
  )

import Json
  ( JsonValue(..)
  )

data Request = ReqExit
             | ReqSelectFrom
             | ReqFindWith
             | ReqSetToIn

pReq :: Parser Request
pReq = do
  _ <- pSpaces
  choice "request"
    [ ReqExit <$ pExit
    ]

isJsonString :: JsonValue -> Bool
isJsonString (JsonString _) = True
isJsonString _              = False

-- | Verify only length of a row, no explicit types
isRowValid :: Int -> JsonValue -> Bool
isRowValid n (JsonArray xs) = n == length xs
isRowValid _ _              = False

isTableValid :: (String, JsonValue) -> Bool
isTableValid (_:_, JsonArray ((JsonArray cols):xs)) =
  foldr ((&&).isJsonString) True cols &&
  foldr ((&&).isRowValid (length cols)) True xs
isTableValid _ = False

-- | Traverse the input Json structure to verify scheme
isDbValid :: JsonValue -> Bool
isDbValid (JsonObject ts) = foldr ((&&).isTableValid) True ts
isDbValid _ = False

-- | Try to apply well-formed request and return new database
tryApplyReq :: Request -> JsonValue -> IO JsonValue
tryApplyReq _ = pure
