{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module Json where

import Parser
  ( Parser
  , choice
  , parseSpaces
  , parseNull
  , parseBool
  , parseQuote
  , parseNumber
  , parseBracesOf
  , parseBracketsOf
  , parseKeyVal
  )

data JsonValue = JsonNull
               | JsonBool   Bool
               | JsonString String
               | JsonNumber Int
               | JsonArray  [JsonValue]
               | JsonObject [(String, JsonValue)]
  deriving (Show)

-- | Parse Json Value from a given string
parseJsonValue :: Parser JsonValue
parseJsonValue =  do
  _ <- parseSpaces
  choice "json value"
    [ JsonNull   <$  parseNull
    , JsonBool   <$> parseBool
    , JsonString <$> parseQuote
    , JsonNumber <$> parseNumber
    , JsonArray  <$> parseBracketsOf parseJsonValue
    , JsonObject <$> parseBracesOf (parseKeyVal parseJsonValue)
    ]
