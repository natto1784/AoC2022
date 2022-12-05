import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (intersect)

main :: IO ()
main = do
  input <- lines <$> readFile "day3.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input

q1 :: [String] -> Int
q1 =
  sum
    . map (\x -> val . head . uncurry intersect $ splitAt (div (length x) 2) x)

q2 :: [String] -> Int
q2 [] = 0
q2 input =
  let (cur, rest) = splitAt 3 input
   in (val . head . foldr1 intersect) cur + q2 rest

val :: Char -> Int
val c
  | isAsciiLower c = 1 + fromEnum c - fromEnum 'a'
  | isAsciiUpper c = 27 + fromEnum c - fromEnum 'A'
  | otherwise = 0
