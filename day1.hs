import Data.List (sortOn)
import Data.Ord (Down (Down))
import Lib (split)

main :: IO ()
main = do
  input <- readFile "day1.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input

q1 :: String -> Int
q1 = maximum . calories

q2 :: String -> Int
q2 = sum . take 3 . sortOn Down . calories

calories :: String -> [Int]
calories =
  map (sum . map read)
    . filter (not . null)
    . split ""
    . split '\n'
