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

jsval2str :: JsonValue -> String -> String
jsval2str j "" = show j
jsval2str j s  = show j <> ", " <> s

jsitem2str :: (String, JsonValue) -> String -> String
jsitem2str (k, v) "" = show k <> ":" <> show v
jsitem2str (k, v) s = show k <> ":" <> show v <> ", " <> s

instance Eq JsonValue where

  (==) JsonNull        JsonNull        = True
  (==) (JsonBool   b1) (JsonBool b2)   = b1 == b2
  (==) (JsonString s1) (JsonString s2) = s1 == s2
  (==) (JsonNumber n1) (JsonNumber n2) = n1 == n2
  (==) (JsonArray  a1) (JsonArray  a2) = a1 == a2
  (==) (JsonObject o1) (JsonObject o2) = and $ zipWith (\ (s1,j1) (s2,j2) -> s1 == s2 && j1 == j2) o1 o2
  (==) _               _               = False

instance Show JsonValue where

  show JsonNull        = "null"
  show (JsonBool    b) = if b then "true" else "false"
  show (JsonString  s) = show s
  show (JsonNumber  n) = show n
  show (JsonArray  xs) = "[" <> foldr jsval2str  "" xs <> "]"
  show (JsonObject xs) = "{" <> foldr jsitem2str "" xs <> "}"

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
