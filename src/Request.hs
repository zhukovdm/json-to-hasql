{-# OPTIONS_GHC -Wall #-}

module Request where

import Data.Char
  ( isLetter
  )

import Data.Maybe
  ( isJust
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
             | ReqSelectFrom ([String], String)
             | ReqFindWith (String, JsonValue)
             | ReqSetToIn
  deriving (Show)

pReqSelectFrom :: Parser ([String], String)
pReqSelectFrom = do
  _ <- matchToken "select"
  c <- many1 il `sepBy` matchToken ","
  _ <- pSpaces
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

-- | Look up key (table name) in a list of tables
findIndex :: [(String, JsonValue)] -> String -> Maybe Int
findIndex [] _     = Nothing
findIndex ((k0,_):ts) k1
  | k0 == k1  = Just 0
  | otherwise = Just (1+) <*> findIndex ts k1

-- | Look up table by key
findTable :: String -> [(String, JsonValue)] -> Maybe JsonValue
findTable _ [] = Nothing
findTable k0 ((k1,t):ts)
  | k0 == k1  = Just t
  | otherwise = findTable k0 ts

extrIndex :: Int -> JsonValue -> JsonValue
extrIndex i (JsonArray xs) = xs !! i
extrIndex _ _ = error "invalid operation"

-- | Extract certain column from the table
extrCol :: [JsonValue] -> Maybe Int -> [JsonValue]
extrCol rows (Just i) = map (extrIndex i) rows
extrCol _ _ = error "invalid operation"

-- | Extract predefined multiple columns from the table
extrMulCols :: [Maybe Int] -> JsonValue -> [[JsonValue]]
extrMulCols ids (JsonArray rows) = foldr ((:).extrCol rows) [] ids
extrMulCols _ _ = error "invalid operation" -- will not happen

-- | Try to apply well-formed request and return new database
tryApplyReq :: Request -> JsonValue -> IO JsonValue
tryApplyReq ReqExit t = pure t

tryApplyReq (ReqSelectFrom (cs, t)) j@(JsonObject ts) = do
  let tb = findTable t ts
  case tb of
    Nothing -> do
      putStrLn "invalid request"
      return j
    Just tx -> do
      let indices = map (findIndex ts) cs
      let valid   = foldr ((&&).isJust) True indices
      if not valid then do
        putStrLn "invalid request"
        return j
      else do      -- columns to be extracted from the (t)able
        let _ = map (print.JsonArray) (extrMulCols indices tx)
        return j

tryApplyReq (ReqFindWith _) t = pure t
tryApplyReq ReqSetToIn      t = pure t
tryApplyReq _               t = pure t
