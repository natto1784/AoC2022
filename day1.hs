import Data.List (sortOn)
import Data.Ord (Down (Down))

main :: IO ()
main = do
  input <- lines <$> readFile "day1.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input

q1 :: [String] -> Int
q1 = maximum . calories

q2 :: [String] -> Int
q2 = sum . take 3 . sortOn Down . calories

calories :: [String] -> [Int]
calories [] = []
calories input =
  let (cur, rest) = break null input
   in sum (map read cur) : calories (drop 1 rest)
