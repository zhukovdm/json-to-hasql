{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module Json where

import Parser
  ( Parser
  , choice
  , parseNull
  , parseBool
  , parseString
  , parseNumber
  , spaces
  )

data JsonValue = JsonNull
               | JsonBool   Bool
               | JsonString String
               | JsonNumber Int
               | JsonArray  [JsonValue]
               | JsonObject [(String, JsonValue)]
  deriving (Show)

-- | Parse Json structure from string
parseJson :: Parser JsonValue
parseJson =  do
  _ <- spaces
  choice "json value"
    [ JsonNull   <$  parseNull
    , JsonBool   <$> parseBool
    , JsonString <$> parseString
    , JsonNumber <$> parseNumber
    ]
