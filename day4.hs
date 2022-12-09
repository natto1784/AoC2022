{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.Either (rights)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T (lines, stripPrefix)
import qualified Data.Text.Read as T (decimal)
import Lib (readFile')

main :: IO ()
main = do
  input <- rights . map parse . T.lines <$> readFile' "day4.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input

parse :: Text -> Either String ((Int, Int), (Int, Int))
parse rest = do
  (a, rest) <- T.decimal rest
  (b, rest) <- T.decimal $ fromJust $ T.stripPrefix "-" rest
  (c, rest) <- T.decimal $ fromJust $ T.stripPrefix "," rest
  (d, rest) <- T.decimal $ fromJust $ T.stripPrefix "-" rest
  return ((a, b), (c, d))

q1, q2 :: [((Int, Int), (Int, Int))] -> Int
q1 = length . filter (\((a, b), (c, d)) -> a >= c && b <= d || c >= a && d <= b)
q2 = length . filter (\((a, b), (c, d)) -> b >= c && a <= d)
