{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S (fromList, insert, map, member, notMember, union)
import Data.Text (Text)
import qualified Data.Text as T (lines, splitOn, words)
import qualified Data.Text.Read as T (decimal)
import Lib (readFile')

-- times can be improved greatly using Data.HashSet in unordered-containers, but i wont use it

main :: IO ()
main = do
  input <- parse . T.lines <$> readFile' "day14.in"
  let mx = maximum $ S.map snd input
  putStr "Q1: "
  print $ sand (500, 0) input 0 mx Q1
  putStr "Q2: "
  print $ sand (500, 0) input 0 (mx + 2) Q2

type Coord = (Int, Int)

data Q = Q1 | Q2 deriving (Eq)

sand :: Coord -> Set Coord -> Int -> Int -> Q -> Int
sand (x, y) rocks soFar mx q
  | (q == Q1 && y > mx) || S.member (x, y) rocks = soFar
  | q == Q2 && y + 1 == mx = sand (500, 0) (S.insert (x, y) rocks) (soFar + 1) mx q
  | S.notMember (x, y + 1) rocks = sand (x, y + 1) rocks soFar mx q
  | S.notMember (x - 1, y + 1) rocks = sand (x - 1, y + 1) rocks soFar mx q
  | S.notMember (x + 1, y + 1) rocks = sand (x + 1, y + 1) rocks soFar mx q
  | otherwise = sand (500, 0) (S.insert (x, y) rocks) (soFar + 1) mx q

parse :: [Text] -> Set Coord
parse =
  S.fromList
    . concatMap
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
    ranges :: [Coord] -> [Coord]
    ranges xs =
      foldr
        ( \((a, b), (x, y)) z ->
            if a == x
              then zip (repeat a) [min b y .. max b y] ++ z
              else zip [min a x .. max a x] (repeat b) ++ z
        )
        []
        (zip xs (tail xs))
