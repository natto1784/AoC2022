module Lib (calories) where

calories :: String -> [Integer]
calories input =
  map (sum . map read) $
    filter (not . null) $ split "" $ split '\n' input

split :: Eq a => a -> [a] -> [[a]]
split del =
  foldr
    ( \c (x : xs) ->
        if c == del
          then [] : x : xs
          else (c : x) : xs
    )
    [[]]
