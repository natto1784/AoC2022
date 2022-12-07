import Data.List (nub)

main :: IO ()
main = do
  input <- readFile "day6.in"
  putStr "Q1: "
  print $ parse 4 input
  putStr "Q2: "
  print $ parse 14 input

group :: Int -> [Char] -> [[Char]]
group _ [] = []
group n xs = take n xs : group n (tail xs)

parse :: Int -> String -> Int
parse n = (+ n) . length . takeWhile ((< n) . length) . map nub . group n
