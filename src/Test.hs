{-# OPTIONS_GHC -Wall #-}

-- this file is created solely for testing purposes

import Parser
  ( parseChar
  , parseEof
  , matchChar
  , parseSpace
  , parseSpaces
  , parseDigit
  , matchWord
  , parseNumber
  , parseNull
  , parseBool
  , parseQuote
  , parseBracesOf
  , parseBracketsOf
  , parseKeyVal
  , run
  )

import Json
  ( parseJsonValue
  )

{------------------------------------------------------------------------------}
{- Parser.hs                                                                  -}
{------------------------------------------------------------------------------}

-- >>> run parseChar "a"
-- Right 'a'

-- >>> run parseEof ""
-- Right ()

-- >>> run (matchChar 'a') "a"
-- Right 'a'

-- >>> run parseSpace " $"
-- Left parse error >> expected: end of file, found: $

-- >>> run parseSpaces "   "
-- Right "   "

-- >>> run parseDigit "0"
-- Right '0'

-- >>> run (matchWord "word") "word$"
-- Left parse error >> expected: end of file, found: $

-- >>> run parseNumber "123 "
-- Right 123

-- >>> run parseNull "null"
-- Left parse error >> expected: n, found:  

-- >>> run parseBool "true"
-- Right True

-- >>> run parseQuote "\"abc\""
-- Right "abc"

-- >>> run (parseBracketsOf parseNumber) "[1, 2, 3]"
-- Right [1,2,3]

-- >>> run (parseBracesOf parseBool) "{ true, false }"
-- Right [True,False]

-- >>> run (parseKeyVal parseNumber) "\"abc\" : 1"
-- Right ("abc",1)

-- >>> run (parseBracesOf (parseKeyVal parseNumber)) "{ \"abc\" : 1 , \"def\" : 2 }"
-- Right [("abc",1),("def",2)]

{------------------------------------------------------------------------------}
{- Json.hs                                                                    -}
{------------------------------------------------------------------------------}

-- >>> run parseJsonValue "null"
-- Right JsonNull

-- >>> run parseJsonValue "true"
-- Right (JsonBool True)

-- >>> run parseJsonValue "\"word\""
-- Right (JsonString "word")

-- >>> run parseJsonValue "123456"
-- Right (JsonNumber 123456)

-- >>> run parseJsonValue "[]"
-- Right (JsonArray [])

-- >>> run parseJsonValue "[null,true,\"a\",1]"
-- Right (JsonArray [JsonNull,JsonBool True,JsonString "a",JsonNumber 1])

-- >>> run parseJsonValue "{ \"abc\" : true, \"def\" : null }"
-- Right (JsonObject [("abc",JsonBool True),("def",JsonNull)])

{------------------------------------------------------------------------------}
{- Sql.hs                                                                     -}
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
{- Main.hs                                                                    -}
{------------------------------------------------------------------------------}

