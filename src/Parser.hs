{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

{------------------------------------------------------------------------------}
{-                                                                            -}
{- This module is almost an exact copy of the Parser module from              -}
{-   https://gist.github.com/jiribenes/0cc7049355bee3b23f7ae947fa53da5e       -}
{-                                                                            -}
{- Parser data type is a function, that takes String and returns rest of the  -}
{- String. Parser either succeeds and returns consumed string with Right X,   -}
{- where x is a parsed structure, or it returns input with Left Error.        -}
{-                                                                            -}
{------------------------------------------------------------------------------}

module Parser where

import Data.Char
  ( isDigit
  , isSpace
  )

data ParseError = ParseError
  { errorExpected :: String
  , errorFound :: String
  }

instance Show ParseError where
  show err = "parse error >> expected: " <> errorExpected err <> ", found: " <> errorFound err

newtype Parser a = Parser
  { runParser :: String -> (String, Either ParseError a)
  }

instance Functor Parser where

  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \ input ->
    case runParser p input of
      (rest, Right x)  -> (rest, Right $ f x)
      (rest, Left err) -> (rest, Left err)

instance Applicative Parser where

  pure :: a -> Parser a
  pure x = Parser $ \ input -> (input, Right x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser $ \input ->
    case runParser p1 input of
      (rest, Right f)  -> runParser (fmap f p2) rest
      (rest, Left err) -> (rest, Left err)

instance Monad Parser where

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \input ->
    case runParser p input of
      (rest, Right x)  -> runParser (f x) rest
      (rest, Left err) -> (rest, Left err)

-- | Introduce error if parser ended up with an error.
introError :: String -> String -> Parser a
introError expected found =
  Parser $ \ input -> (input, Left $ ParseError expected found)

-- | Try parse with backtracking
tryParse :: Parser a -> Parser a
tryParse p = Parser $ \input ->
  case runParser p input of
    (_, Left err) -> (input, Left err) -- no input reduction ~> backtrack
    success       -> success

-- | Combine two parsers, try 2nd if 1st fails.
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \ input ->
  case runParser p1 input of
    (rest, Left err)
      | rest == input -> runParser p2 input
      | otherwise     -> (rest, Left err)
    success -> success

-- | Combine list of parsers, apply one by one
choice :: String -> [Parser a] -> Parser a
choice desc = foldr (<|>) noMatch
  where
    noMatch = introError desc "no match"

-- | Parse any char, if remained. Otherwise, error upon Eof.
pChar :: Parser Char
pChar = Parser go
  where
    go :: String -> (String, Either ParseError Char)
    go input = case input of
      []     -> ("", Left $ ParseError "any character" "end of file")
      (x:xs) -> (xs, Right x)

-- | Parse EoF (no chars ~ empty input)
pEof :: Parser ()
pEof = Parser go
  where
    go :: String -> (String, Either ParseError ())
    go input = case input of
      []    -> (""   , Right ())
      (c:_) -> (input, Left $ ParseError "end of file" [c])

-- | Run parser on the input, returns either structure or error
run :: Parser a -> String -> Either ParseError a
run p s = snd $ runParser go s
  where
    go = do
      result <- p
      pEof
      return result

-- | Run input parser at least once
many1 :: Parser a -> Parser [a]
many1 p = do
  first <- p
  rest  <- many p
  return (first : rest)

-- | Restart parser in a sequence 0-to-n times
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- | Satisfy predicate on the char, otherwise error.
satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy desc p = tryParse $ do
  c <- pChar
  if p c
    then return c
    else introError desc [c]

-- | Match a given char
matchChar :: Char -> Parser Char
matchChar c = satisfy [c] (== c)

-- | Parse a white sign (space, tab, etc.)
pSpace :: Parser Char
pSpace = satisfy "space" isSpace

-- | Parse multiple spaces
pSpaces :: Parser String
pSpaces = many pSpace

-- | Parse a digit [0..9]
pDigit :: Parser Char
pDigit = satisfy "digit" isDigit

-- | Match a given word
matchWord :: String -> Parser String
matchWord = mapM matchChar

-- | Convert string-to-integer
str2int :: String -> Int
str2int = read

-- | Parse multiple digits into a number
pNumber :: Parser Int
pNumber = do
  numstr <- many1 pDigit
  _      <- pSpaces
  return $  str2int numstr

-- | Match a given token (key words, such as null, etc.)
matchToken :: String -> Parser String
matchToken w = do
  r <- matchWord w
  _ <- pSpaces -- remove spaces after
  return r

-- | Parse "null" token
pNull :: Parser ()
pNull = do
  _ <- matchToken "null"
  return ()

-- | Parse "quit" token
pQuit :: Parser ()
pQuit = do
  _ <- matchToken "quit"
  return ()

-- | Parse "true" token
pTrue :: Parser Bool
pTrue = do
  _ <- matchToken "true"
  return True

-- | Parse "false" token
pFalse :: Parser Bool
pFalse = do
  _ <- matchToken "false"
  return False

-- | Parse bool (either "true" or "false")
pBool :: Parser Bool
pBool = do
  choice "bool" [pTrue, pFalse]

-- | Parse left-between-right
between :: Parser a -> Parser c -> Parser b -> Parser b
between left right p = do
  _ <- left
  r <- p
  _ <- right
  return r

-- | Parse quote inside \"  \", dittoes inside are not allowed
pQuote :: Parser String
pQuote = do
  r <- between (matchChar '"') (matchChar '"') (many notQuote)
  _ <- pSpaces
  return r
    where
      notQuote = satisfy "not a quote" (/= '"')

-- | Parse content within [..]
brackets :: Parser a -> Parser a
brackets = between (matchToken "[") (matchToken "]")

-- | Parse content within {..}
braces :: Parser a -> Parser a
braces = between (matchToken "{") (matchToken "}")

-- | Parse items divided by separator by corr. parsers at least once.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p s = do
  first <- p
  rest  <- many (s >> p)
  return (first : rest)

-- | Parse items divided by separator by corr. parsers, apply 0-to-n times
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p s = sepBy1 p s <|> return []

-- | Parse items inside [.., ..] separated by a comma
pBracketsOf :: Parser a -> Parser [a]
pBracketsOf p = brackets $ p `sepBy` matchToken ","

-- | Parse items within {.., ..} separated by a comma
pBracesOf :: Parser a -> Parser [a]
pBracesOf p = braces $ p `sepBy` matchToken ","

-- | Parse key-value separated by a colon
pKeyVal :: Parser a -> Parser (String, a)
pKeyVal p = do
  key <- pQuote
  _   <- matchToken ":"
  val <- p
  return (key, val)
