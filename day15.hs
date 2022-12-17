{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List (nub, sort)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T (lines, null, split)
import qualified Data.Text.Read as T (decimal, signed)
import Lib (readFile')

main :: IO ()
main = do
  input <- map parse . T.lines <$> readFile' "day15.in"
  putStr "Q1: "
  print $ q1 input 2000000
  putStr "Q2: "
  print $ q2 input 0 4000000

type Coords = (Int, Int, Int, Int, Int)

q1 :: [Coords] -> Int -> Int
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

type Segment = (Int, Int, Int, Int)

q2 input mn mx =
  head
    [ a * mx + b
      | (a, b) <- diag1234,
        a >= mn && b >= mn && a <= mx && b <= mx,
        and [abs (b - y) + abs (a - x) > r | (x, y, r, _, _) <- input]
    ]
  where
    diag1, diag2, diag3, diag4 :: Coords -> Segment
    diag1 (x, y, r, _, _) = (x - r - 1, y, x, y - r - 1)
    diag2 (x, y, r, _, _) = (x, y - r - 1, x + r + 1, y)
    diag3 (x, y, r, _, _) = (x, y + r + 1, x + r + 1, y)
    diag4 (x, y, r, _, _) = (x - r - 1, y, x, y + r + 1)

    diagInt :: Segment -> Segment -> Maybe Segment
    diagInt a@(x0, y0, x1, y1) b@(x2, y2, x3, y3)
      | x3 < x0 || x1 < x2 = Nothing
      | x0 >= x2 && x1 <= x3 = Just a
      | x2 >= x0 && x3 <= x1 = Just b
      | x3 >= x1 = Just (x2, y2, x1, y1)
      | x1 >= x3 = Just (x0, y0, x3, y3)

    diag13, diag24 :: [Segment]
    diag13 =
      catMaybes
        [ diagInt i j
          | a <- input,
            b <- input,
            let i@(x0, y0, x1, y1) = diag1 a,
            let j@(x2, y2, x3, y3) = diag3 b,
            a /= b,
            x0 + y0 == x2 + y2
        ]
    diag24 =
      catMaybes
        [ diagInt i j
          | a <- input,
            b <- input,
            let i@(x0, y0, x1, y1) = diag2 a,
            let j@(x2, y2, x3, y3) = diag4 b,
            a /= b,
            y0 - x0 == y2 - x2
        ]

    diag1234 :: [(Int, Int)]
    diag1234 =
      [ let (c1, c2) = (y0 + x0, y2 - x2) in (div (c1 - c2) 2, div (c1 + c2) 2)
        | (x0, y0, x1, y1) <- diag13,
          (x2, y2, x3, y3) <- diag24
      ]

parse :: Text -> Coords
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
