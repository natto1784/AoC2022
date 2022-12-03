module Lib (chunks, split) where

chunks :: Int -> [a] -> [[a]]
chunks n xs
  | null xs = []
  | otherwise = head : chunks n tail
  where (head, tail) = splitAt n xs

split :: Eq a => a -> [a] -> [[a]]
split del =
  foldr
    ( \c (x : xs) ->
        if c == del
          then [] : x : xs
          else (c : x) : xs
    )
    [[]]
