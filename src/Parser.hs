{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

{------------------------------------------------------------------------------}
{-                                                                            -}
{- This module is an exact copy of the Parser module from                     -}
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

-- | Parser ended up with an error, construct error
parseError :: String -> String -> Parser a
parseError expected found =
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

-- | Parse any character, if remained. Otherwise, error upon Eof.
parseAny :: Parser Char
parseAny = Parser go
  where
    go :: String -> (String, Either ParseError Char)
    go input = case input of
      []     -> ("", Left $ ParseError "any character" "end of file")
      (x:xs) -> (xs, Right x)

-- | Parser succeeds if EoF is found.
parseEof :: Parser ()
parseEof = Parser go
  where
    go :: String -> (String, Either ParseError ())
    go input = case input of
      []    -> (""   , Right ())
      (c:_) -> (input, Left $ ParseError "end of file" [c])

-- | Try to satisfy predicate on the letter, otherwise error.
satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy desc p = tryParse $ do
  c <- parseAny
  if p c
    then return c
    else parseError desc [c]

-- | Run parser on the input, returns either structure or error.
run :: Parser a -> String -> Either ParseError a
run p s = snd $ runParser go s
  where
    go = do
      result <- p
      parseEof
      return result

-- | Parse input char
char :: Char -> Parser Char
char c = satisfy [c] (== c)

-- >>> run (char 'a') "a"
-- Right 'c'

-- | Parse any white sign (space, tab, etc.)
space :: Parser Char
space = satisfy "space" isSpace

-- >>> run space " "
-- Right ' '

-- | Parse any digit [0..9]
digit :: Parser Char
digit = satisfy "digit" isDigit

-- >>> run digit "0"
-- Right '0'

-- | Restart parser in a sequence 0-to-n times
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- | Run input parser at least once
many1 :: Parser a -> Parser [a]
many1 p = do
  first <- p
  rest  <- many p
  return (first : rest)

-- >>> run (many digit) ""
-- Right ""

-- >>> run (many digit) "123456"
-- Right "123456"

-- | Try to parse a given string
string :: String -> Parser String
string = mapM char

-- >>> run (string "word") "word"
-- Right "word"

-- | Convert string to integer
str2int :: String -> Int
str2int = read

-- >>> str2int "123"
-- 123

-- | Parse multiple digits into number
parseNumber :: Parser Int
parseNumber = do
  numstr <- many1 digit
  return $ str2int numstr

-- >>> run number "123"
-- Right 123

-- | Parse multiple spaces
spaces :: Parser String
spaces = many space

-- >>> run spaces "   "
-- Right "   "

-- | parse symbol ~ string, drop spaces after till end of file.
symbol :: String -> Parser String
symbol s = do
  result <- string s
  _      <- spaces
  return result

-- >>> run (symbol "word") "word   "
-- Right "word"

-- >>> run (symbol "word") "word  $"
-- Left expected: end of file, found: $

-- | Parse left, between, right
between :: Parser a -> Parser c -> Parser b -> Parser b
between left right p = do
  _      <- left
  result <- p
  _      <- right
  return result

-- | Parse content between ()
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- >>> run (parens (many digit)) "(123)"
-- Right "123"

-- | Parse content between []
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Parse content between {}
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parse "null" symbol
parseNull :: Parser ()
parseNull = do
  _ <- symbol "null"
  return ()

-- >>> run parseNull "null"
-- Right ()

-- | Parse "true" symbol
parseTrue :: Parser Bool
parseTrue = do
  _ <- symbol "true"
  return True

-- | Parse "false" symbol
parseFalse :: Parser Bool
parseFalse = do
  _ <- symbol "false"
  return False

-- | Parse bool (either "true" or "false") symbol
parseBool :: Parser Bool
parseBool = do
  _ <- spaces
  choice "bool" [parseTrue, parseFalse]

-- | Parse string inside \"  \"
parseString :: Parser String
parseString = between (char '"') (char '"') (many parseChar)
  where
    parseChar = satisfy "not a quote" (/= '"')

-- >>> run parseString "\"word\""
-- Right "word"

-- | Parse items divided by separator by corr. parsers, apply 0-to-n times
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy  p s = sepBy1 p s <|> return []

-- | Parse items divided by separator by corr. parsers at least once.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p s = do
  first <- p
  rest  <- many (s >> p)
  return (first : rest)

-- | Parse list of items in [] divided by comma
parseListOf :: Parser a -> Parser [a]
parseListOf p = brackets $ p `sepBy` symbol ","

-- >>> run (parseListOf number) "[1,2]"
-- Right [1,2]

-- | Combine list of parsers, apply one by one
choice :: String -> [Parser a] -> Parser a
choice desc = foldr (<|>) noMatch
  where
    noMatch = parseError desc "no match"
