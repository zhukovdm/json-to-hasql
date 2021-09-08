{-# OPTIONS_GHC -Wall #-}

-- General purpose functions

module General
  ( joinArr
  ) where

joinArr :: (Show a) => (a -> String) -> String -> [a] -> String
joinArr _ _ []     = ""
joinArr f _ [x]    = f x
joinArr f s (x:xs) = f x <> s <> joinArr f s xs
