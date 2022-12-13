{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Char (isDigit)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T (filter, init, lines, null, span, tail, uncons)
import qualified Data.Text.Read as T (decimal)
import Lib (readFile')

main :: IO ()
main = do
  input <-
    map (fst . parse (List []) . T.init . T.tail) . filter (not . T.null)
      . T.lines
      <$> readFile' "day13.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input (List [List [Elem 2]]) (List [List [Elem 6]])

q1 :: [List] -> Int
q1 input =
  foldl
    (\xs x -> xs + if input !! (2 * x - 2) < input !! (2 * x - 1) then x else 0)
    0
    [1 .. length input `div` 2]

q2 :: [List] -> List -> List -> Int
q2 input a b =
  let sorted = sort (a : b : input)
      a' = fromJust $ elemIndex a sorted
      b' = fromJust $ elemIndex b sorted
   in (a' + 1) * (b' + 1)

data List = Elem Int | List [List] deriving (Show, Eq)

-- is this cheating :P
instance Ord List where
  compare (List xs) (List ys) = compare (reverse xs) (reverse ys)
  compare (List xs) (Elem y) = compare (List xs) (List [Elem y])
  compare (Elem x) (List ys) = compare (List [Elem x]) (List ys)
  compare (Elem x) (Elem y) = compare x y

-- Weak parser but it works
-- I do not really know how to use Parsec and ReadP with Data.Text yet

parse :: List -> Text -> (List, Text)
parse (List xs) txt
  | T.null txt || cur == ']' = (List xs, rest)
  | cur == '[' =
    let (new, next) = parse (List []) rest
     in parse (List (new : xs)) next
  | isDigit cur = parse (List (Elem num : xs)) rest'
  | otherwise = parse (List xs) rest
  where
    (cur, rest) = fromJust $ T.uncons txt
    (T.decimal -> Right (num, ""), rest') = T.span isDigit txt
