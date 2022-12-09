{-# LANGUAGE OverloadedStrings #-}

import Data.List (nub)
import Lib (readFile')
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  input <- readFile' "day6.in"
  putStr "Q1: "
  print $ parse 4 input
  putStr "Q2: "
  print $ parse 14 input

group :: Int -> Text -> [Text]
group _ "" = []
group n xs = T.take n xs : group n (T.tail xs)

parse :: Int -> Text -> Int
parse n = (+ n) . length . takeWhile ((< n) . length) . map (nub . T.unpack) . group n
