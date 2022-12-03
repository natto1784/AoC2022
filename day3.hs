import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (intersect)
import Lib (chunks)

main :: IO ()
main = do
  input <- readFile "day3.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input

q1 :: String -> Int
q1 =
  sum
    . map (\x -> val . head . uncurry intersect $ splitAt (div (length x) 2) x)
    . lines

q2 :: String -> Int
q2 =
  sum
    . map (val . head . foldr1 intersect)
    . chunks 3
    . lines

val :: Char -> Int
val c
  | isAsciiLower c = 1 + fromEnum c - fromEnum 'a'
  | isAsciiUpper c = 27 + fromEnum c - fromEnum 'A'
  | otherwise = 0
