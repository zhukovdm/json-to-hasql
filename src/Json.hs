{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module Json where

import General
  ( joinArr
  )

import Parser
  ( Parser
  , choice
  , pSpaces
  , pNull
  , pBool
  , pQuote
  , pNumber
  , pBracesOf
  , pBracketsOf
  , pKeyVal
  , run
  )

data JsonValue = JsonNull
               | JsonBool   Bool
               | JsonString String
               | JsonNumber Int
               | JsonArray  [JsonValue]
               | JsonObject [(String, JsonValue)]

showJsonItem :: (String, JsonValue) -> String
showJsonItem (k, v) = show k <> ":" <> show v

instance Show JsonValue where

  show JsonNull        = "null"
  show (JsonBool    b) = if b then "true" else "false"
  show (JsonString  s) = show s
  show (JsonNumber  n) = show n
  show (JsonArray  xs) = "[" <> joinArr show         "," xs <> "]"
  show (JsonObject xs) = "{" <> joinArr showJsonItem "," xs <> "}"

-- | Parse Json Value from a given string
pJsonValue :: Parser JsonValue
pJsonValue = do
  _ <- pSpaces
  choice "json value"
    [ JsonNull   <$  pNull
    , JsonBool   <$> pBool
    , JsonString <$> pQuote
    , JsonNumber <$> pNumber
    , JsonArray  <$> pBracketsOf pJsonValue
    , JsonObject <$> pBracesOf (pKeyVal pJsonValue)
    ]

parseJson :: String -> IO (Either String JsonValue)
parseJson s = do
  let r = run pJsonValue s
  case r of
    (Left err) -> return $ Left (show err)
    (Right jv) -> return $ Right jv
