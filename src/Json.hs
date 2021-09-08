{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module Json where

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
  deriving (Show)

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
    (Left  e) -> return $ Left (show e)
    (Right v) -> return $ Right v
