module Lib (split) where

split :: Eq a => a -> [a] -> [[a]]
split del =
  foldr
    ( \c (x : xs) ->
        if c == del
          then [] : x : xs
          else (c : x) : xs
    )
    [[]]
