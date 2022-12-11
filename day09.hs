{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List (nub, scanl)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack, words)
import qualified Data.Text.Read as T (decimal)
import Lib (readFile')

-- a rather slow program, takes 300 ms on my system (with `time`, but idc anymore 😭

main :: IO ()
main = do
  input <-
    concatMap
      ((\[T.unpack -> [d], T.decimal -> Right (n, "")] -> replicate n d) . T.words)
      . T.lines
      <$> readFile' "day9.in"
  putStr "Q1: "
  print $ q 1 input
  putStr "Q2: "
  print $ q 9 input

moveHead :: Char -> (Int, Int) -> (Int, Int)
moveHead d (x, y) = case d of
  'L' -> (x - 1, y)
  'R' -> (x + 1, y)
  'U' -> (x, y + 1)
  'D' -> (x, y - 1)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (hx, hy) (tx, ty)
  | abs dx <= 1 && abs dy <= 1 = (tx, ty) -- dont move
  | abs dx > abs dy = (tx + signum dx, hy) -- move horizontally
  | abs dy > abs dx = (hx, ty + signum dy) -- move vertically
  | otherwise = (tx + signum dx, ty + signum dy ) -- move diagonally
  where
    dx = hx - tx
    dy = hy - ty

q :: Int -> [Char] -> Int
q n =
  length . nub . map last
    . scanl
      ( \(h : t) d ->
          reverse $ foldl (\(h : xs) x -> moveTail h x : (h : xs)) [moveHead d h] t
      )
      (replicate (n + 1) (0, 0))
