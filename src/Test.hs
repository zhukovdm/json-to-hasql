{-# OPTIONS_GHC -Wall #-}

-- this file is created solely for testing purposes

import Parser
  ( char
  , digit
  , many
  , space
  , spaces
  , string
  , str2int
  , symbol
  , parens
  , parseNull
  , parseBool
  , parseString
  , parseNumber
  , parseListOf
  , run
  )

import Json
  ( parseJson
  )

{------------------------------------------------------------------------------}
{- Parser.hs                                                                  -}
{------------------------------------------------------------------------------}

-- >>> run (char 'a') "a"
-- Right 'a'

-- >>> run space " "
-- Right ' '

-- >>> run digit "0"
-- Right '0'

-- >>> run (many digit) ""
-- Right ""

-- >>> run (many digit) "123456"
-- Right "123456"

-- >>> run (string "word") "word"
-- Right "word"

-- >>> str2int "123"
-- 123

-- >>> run parseNumber "123"
-- Right 123

-- >>> run spaces "   "
-- Right "   "

-- >>> run (symbol "word") "word   "
-- Right "word"

-- >>> run (symbol "word") "word  $"
-- Left Parse error >> Expected: end of file, Found: $

-- >>> run (parens (many digit)) "(123)"
-- Right "123"

-- >>> run parseNull "null"
-- Right ()

-- >>> run parseBool "true"
-- Right True

-- >>> run parseString "\"word\""
-- Right "word"

-- >>> run parseNumber "123"
-- Right 123

-- >>> run (parseListOf parseNumber) "[1,2]"
-- Right [1,2]

{------------------------------------------------------------------------------}
{- Json.hs                                                                    -}
{------------------------------------------------------------------------------}

-- >>> run parseJson "null"
-- Right JsonNull

-- >>> run parseJson "true"
-- Right (JsonBool True)

-- >>> run parseJson "\"word\""
-- Right (JsonString "word")

-- >>> run parseJson "123456"
-- Right (JsonNumber 123456)

{------------------------------------------------------------------------------}
{- Sql.hs                                                                     -}
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
{- Main.hs                                                                    -}
{------------------------------------------------------------------------------}

