{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S (fromList, insert, lookupMax, member, notMember, union)
import Data.Text (Text)
import qualified Data.Text as T (lines, splitOn, words)
import qualified Data.Text.Read as T (decimal)
import Lib (readFile')


-- ok this is it, this takes 4.54s, kms
-- this can probably be improved using hashtables in ST monad but will try that later

main :: IO ()
main = do
  input <- parse . T.lines <$> readFile' "day14.in"
  let mx = fst . fromJust $ S.lookupMax input
  putStr "Q1: "
  print $ sand (0, 500) input 0 mx Q1
  putStr "Q2: "
  print $ sand (0, 500) input 0 (mx + 2) Q2

data Q = Q1 | Q2 deriving (Eq)

sand :: (Int, Int) -> Set (Int, Int) -> Int -> Int -> Q -> Int
sand (y, x) rocks soFar mx q
  | (q == Q1 && y > mx) || S.member (y, x) rocks = soFar
  | q == Q2 && y + 1 == mx = sand (0, 500) (S.insert (y, x) rocks) (soFar + 1) mx q
  | S.notMember (y + 1, x) rocks = sand (y + 1, x) rocks soFar mx q
  | S.notMember (y + 1, x - 1) rocks = sand (y + 1, x - 1) rocks soFar mx q
  | S.notMember (y + 1, x + 1) rocks = sand (y + 1, x + 1) rocks soFar mx q
  | otherwise = sand (0, 500) (S.insert (y, x) rocks) (soFar + 1) mx q

parse :: [Text] -> Set (Int, Int)
parse =
  S.fromList
    . concat
    . map
      ( ranges
          . map
            ( (\[x, y] -> (x, y))
                . map (\(T.decimal -> Right (n, "")) -> n)
                . T.splitOn ","
            )
          . filter (/= "->")
          . T.words
      )
  where
    ranges :: [(Int, Int)] -> [(Int, Int)]
    ranges xs =
      concat $
        foldr
          ( \((a, b), (x, y)) z ->
              if a == x
                then zip [min b y .. max b y] (repeat a) : z
                else zip (repeat b) [min a x .. max a x] : z
          )
          []
          (zip xs (tail xs))
