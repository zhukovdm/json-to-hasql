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

import Request
  ( pReq
  , findTable
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
-- Right null

-- >>> run pJsonValue "true"
-- Right true

-- >>> run pJsonValue "\"word\""
-- Right "word"

-- >>> run pJsonValue "123456"
-- Right 123456

-- >>> run pJsonValue "[]"
-- Right []

-- >>> run pJsonValue "[null,true,\"a\",0]"
-- Right [null, true, "a", 0]

-- >>> run pJsonValue "{ \"abc\" : true, \"def\" : null }"
-- Right {"abc":true, "def":null}

{------------------------------------------------------------------------------}
{- Request.hs                                                                 -}
{------------------------------------------------------------------------------}

-- >>> run pReq "select color from fruits"
-- Right (ReqSelectFrom (["color"],"fruits"))

{------------------------------------------------------------------------------}
{- Main.hs                                                                    -}
{------------------------------------------------------------------------------}

