{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T (lines, null, split)
import qualified Data.Text.Read as T (decimal, signed)
import Lib (readFile')

-- broh this is so slow ~ 20s, -O2 is less than 2s though???????
-- will optimize this later, along with last 10 days *sigh*

main :: IO ()
main = do
  input <- map parse . T.lines <$> readFile' "day15.in"
  putStr "Q1: "
  print $ q1 input 2000000
  putStr "Q2: "
  print $ q2 input input 0 4000000

type Entity = (Int, Int, Int, Int, Int)

q1 :: [Entity] -> Int -> Int
q1 input n = mergeRanges getRanges
  where
    mergeRanges :: [(Int, Int)] -> Int
    mergeRanges [(x0, x1)] = x1 - x0 + 1
    mergeRanges ((x0, x1) : (x2, x3) : xs)
      | x2 - x1 < 2 = mergeRanges $ (x0, max x1 x3) : xs
      | otherwise = x1 - x0 + 1 + mergeRanges ((x2, x3) : xs)

    getRanges :: [(Int, Int)]
    getRanges =
      sort $
        foldr
          ( \(x, y, r, a, b) xs -> case r - abs (n - y) of
              dx
                | dx < 0 -> xs
                | n == b -> (x - dx, a - 1) : (a + 1, x + dx) : xs
                | otherwise -> (x - dx, x + dx) : xs
          )
          []
          input

q2 :: [Entity] -> [Entity] -> Int -> Int -> Int
q2 input@((x, y, succ -> e, a, b) : xs) all mn mx =
  let (x', y') = dirs [(-1, -1), (1, -1), (1, 1), (-1, 1)]
   in if (x', y') == (mn - 1, mn - 1) then q2 xs all mn mx else x' * mx + y'
  where
    dirs :: [(Int, Int)] -> (Int, Int)
    dirs [] = (mn - 1, mn - 1)
    dirs ((dx, dy) : ds) =
      let b = border (dx, dy) (if dx * dy > 0 then [0 .. e - 1] else [e, e - 1 .. 1])
       in if b == (mn - 1, mn - 1) then dirs ds else b

    border :: (Int, Int) -> [Int] -> (Int, Int)
    border _ [] = (mn - 1, mn - 1)
    border (dx, dy) (b : bs) =
      let (x', y') = (x + dx * b, y + dy * (e - b))
       in if not (contains (x', y') all) && x' >= mn && x' <= mx && y' >= mn && y' <= mx
            then (x', y')
            else border (dx, dy) bs

    contains :: (Int, Int) -> [Entity] -> Bool
    contains (a', b') = any (\(x, y, r, a, b) -> r >= abs (b' - y) + abs (a' - x))

parse :: Text -> Entity
parse =
  ( \[ "Sensor",
       "at",
       "x",
       T.signed T.decimal -> Right (x, ""),
       "y",
       T.signed T.decimal -> Right (y, ""),
       "closest",
       "beacon",
       "is",
       "at",
       "x",
       T.signed T.decimal -> Right (a, ""),
       "y",
       T.signed T.decimal -> Right (b, "")
       ] -> (x, y, abs (b - y) + abs (a - x), a, b)
  )
    . filter (not . T.null)
    . T.split (`elem` (":=, " :: [Char]))
