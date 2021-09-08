{-# OPTIONS_GHC -Wall #-}

-- this file is created solely for testing purposes

import Parser
  ( pChar
  , pEof
  , matchChar
  , pSpace
  , pSpaces
  , pDigit
  , matchWord
  , pNumber
  , pNull
  , pBool
  , pQuote
  , pBracesOf
  , pBracketsOf
  , pKeyVal
  , run
  )

import Json
  ( pJsonValue
  )

{------------------------------------------------------------------------------}
{- Parser.hs                                                                  -}
{------------------------------------------------------------------------------}

-- >>> run pChar "a"
-- Right 'a'

-- >>> run pEof ""
-- Right ()

-- >>> run (matchChar 'a') "a"
-- Right 'a'

-- >>> run pSpace " $"
-- Left parse error >> expected: end of file, found: $

-- >>> run pSpaces "   "
-- Right "   "

-- >>> run pDigit "0"
-- Right '0'

-- >>> run (matchWord "word") "word$"
-- Left parse error >> expected: end of file, found: $

-- >>> run pNumber "123 "
-- Right 123

-- >>> run pNull "null"
-- Right ()

-- >>> run pBool "true"
-- Right True

-- >>> run pQuote "\"abc\""
-- Right "abc"

-- >>> run (pBracketsOf pNumber) "[1, 2, 3]"
-- Right [1,2,3]

-- >>> run (pBracesOf pBool) "{ true, false }"
-- Right [True,False]

-- >>> run (pKeyVal pNumber) "\"abc\" : 1"
-- Right ("abc",1)

-- >>> run (pBracesOf (pKeyVal pNumber)) "{ \"abc\" : 1 , \"def\" : 2 }"
-- Right [("abc",1),("def",2)]

{------------------------------------------------------------------------------}
{- Json.hs                                                                    -}
{------------------------------------------------------------------------------}

-- >>> run pJsonValue "null"
-- Right JsonNull

-- >>> run pJsonValue "true"
-- Right (JsonBool True)

-- >>> run pJsonValue "\"word\""
-- Right (JsonString "word")

-- >>> run pJsonValue "123456"
-- Right (JsonNumber 123456)

-- >>> run pJsonValue "[]"
-- Right (JsonArray [])

-- >>> run pJsonValue "[null,true,\"a\",0]"
-- Right (JsonArray [JsonNull,JsonBool True,JsonString "a",JsonNumber 0])

-- >>> run pJsonValue "{ \"abc\" : true, \"def\" : null }"
-- Right (JsonObject [("abc",JsonBool True),("def",JsonNull)])

{------------------------------------------------------------------------------}
{- Sql.hs                                                                     -}
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
{- Main.hs                                                                    -}
{------------------------------------------------------------------------------}

