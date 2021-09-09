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
  , choice, pNumber
  )

import Json
  ( JsonValue(..)
  , pJsonValue
  )

data Request = ReqShow String
             | ReqPick ([String], String)             -- columns selection
             | ReqFind (String, JsonValue)            -- rows selection
             | ReqBulk (JsonValue, JsonValue, String) -- bulk change
             | ReqCadd (String, String)               -- add column
             | ReqRadd (String, String)               -- add row
             | ReqEvic (Int, String)                  -- evict row
             | ReqYank (Int, String)                  -- yank column
             | ReqModi (Int, Int, JsonValue, String)  -- modify position
             | ReqExit
  deriving (Show)

il :: Parser Char
il = satisfy "is letter" isLetter

pReqShow :: Parser String
pReqShow = do
  _ <- matchToken "show"
  many1 il

pReqPick :: Parser ([String], String)
pReqPick = do
  _ <- matchToken "pick"
  c <- many1 il `sepBy` matchToken ","
  _ <- pSpaces
  _ <- matchToken "from"
  t <- many1 il
  _ <- pSpaces
  return (c, t)

pReqFind :: Parser (String, JsonValue)
pReqFind = do
  _ <- matchToken "find"
  t <- many1 il
  _ <- pSpaces
  _ <- matchToken "with"
  j <- pJsonValue
  return (t, j)

pReqBulk :: Parser (JsonValue, JsonValue, String)
pReqBulk = do
  _  <- matchToken "bulk"
  fr <- pJsonValue
  _  <- matchToken "to"
  to <- pJsonValue
  _  <- matchToken "in"
  tb <- many1 il
  _  <- pSpaces
  return (fr, to, tb)

pReqCadd :: Parser (String, String)
pReqCadd = do
  _ <- matchToken "cadd"
  c <- many1 il
  _ <- pSpaces
  _ <- matchToken "into"
  t <- many1 il
  _ <- pSpaces
  return (c, t)

pReqRadd :: Parser (String, String)
pReqRadd = do
  _ <- matchToken "radd"
  c <- many1 il
  _ <- pSpaces
  _ <- matchToken "into"
  t <- many1 il
  _ <- pSpaces
  return (c, t)

pReqEvic :: Parser (Int, String)
pReqEvic = do
  _ <- matchToken "evic"
  n <- pNumber
  _ <- matchToken "from"
  t <- many1 il
  _ <- pSpaces
  return (n, t)

pReqYank :: Parser (Int, String)
pReqYank = do
  _ <- matchToken "yank"
  n <- pNumber
  _ <- matchToken "from"
  t <- many1 il
  _ <- pSpaces
  return (n, t)

pReqModi :: Parser (Int, Int, JsonValue, String)
pReqModi = do
  _ <- matchToken "modi"
  r <- pNumber
  c <- pNumber
  _ <- matchToken "to"
  j <- pJsonValue
  _ <- matchToken "in"
  t <- many1 il
  _ <- pSpaces
  return (r, c, j, t)

pReq :: Parser Request
pReq = do
  _ <- pSpaces
  choice "request"
    [ ReqShow <$> pReqShow
    , ReqPick <$> pReqPick
    , ReqFind <$> pReqFind
    , ReqBulk <$> pReqBulk
    , ReqCadd <$> pReqCadd
    , ReqRadd <$> pReqRadd
    , ReqEvic <$> pReqEvic
    , ReqYank <$> pReqYank
    , ReqModi <$> pReqModi
    , ReqExit <$  pExit
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
findIndex :: [JsonValue] -> String -> Maybe Int
findIndex (JsonString k0:ts) k1
  | k0 == k1  = Just 0
  | otherwise = Just (1+) <*> findIndex ts k1
findIndex _ _ = Nothing

-- | Extract array from JsonArray
extractArr :: JsonValue -> [JsonValue]
extractArr (JsonArray xs) = xs
extractArr _              = error "invalid operation"

-- | Look up table by key
findTable :: String -> [(String, JsonValue)] -> Maybe JsonValue
findTable _ [] = Nothing
findTable k0 ((k1,t):ts)
  | k0 == k1  = Just t
  | otherwise = findTable k0 ts

-- | Extract item on a certain index
extrIndex :: Int -> JsonValue -> JsonValue
extrIndex i (JsonArray xs) = xs !! i
extrIndex _ _ = error "invalid operation" -- will not happen

-- | Extract certain column from the table
extrCol :: [JsonValue] -> Maybe Int -> [JsonValue]
extrCol rows (Just i) = map (extrIndex i) rows
extrCol _ _ = error "invalid operation"

-- | Extract predefined multiple columns from the table
extrMulCols :: [Maybe Int] -> JsonValue -> [[JsonValue]]
extrMulCols ids (JsonArray rows) = foldr ((:).extrCol rows) [] ids
extrMulCols _ _ = error "invalid operation" -- will not happen

-- | Check if JsonValue is inside another JsonValue
inside :: JsonValue -> JsonValue -> Bool
inside j (JsonArray xs) = j `elem` xs
inside _ _ = False

-- | Pretty printing
printList :: [JsonValue] -> IO ()
printList [] = pure ()
printList (x:xs) = do
  print x
  printList xs

-- | Try to apply well-formed request and return new database
tryApplyReq :: Request -> JsonValue -> IO JsonValue
tryApplyReq ReqExit t = pure t

tryApplyReq (ReqShow tableName) j@(JsonObject ts) = do
  let t = findTable tableName ts
  case t of
    Nothing -> do
      putStrLn "invalid request, table not found"
      return j
    Just x  -> do
      let arr = extractArr x
      printList arr
      return j

tryApplyReq (ReqPick (cols, tr)) j@(JsonObject ts) = do
  let t = findTable tr ts
  case t of
    Nothing -> do
      putStrLn "invalid request, table not found"
      return j
    Just c -> do
      let rows    = extractArr c     -- rows is a list with JsonArrays
      let indices = map (findIndex (extractArr (head rows))) cols
      let valid   = foldr ((&&).isJust) True indices
      if not valid then do
        putStrLn "invalid request, bad columns"
        return j
      else do      -- columns to be extracted from the (t)able
        let x = map JsonArray (extrMulCols indices c)
        printList x
        return j

tryApplyReq (ReqFind (tableName, jv)) j@(JsonObject ts) = do
  let t = findTable tableName ts
  case t of
    Nothing -> do
      putStrLn "invalid request, table not found"
      return j
    Just  c -> do
      let rows = extractArr c
      let x = filter (inside jv) rows
      printList x
      return j

tryApplyReq _               t = pure t
