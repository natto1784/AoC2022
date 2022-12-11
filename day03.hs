import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (intersect)
import Data.Text (Text)
import qualified Data.Text as T (foldl, lines, splitAt)
import Lib (readFile')

main :: IO ()
main = do
  input <- map parse . T.lines <$> readFile' "day3.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input

q1 :: [[Int]] -> Int
q1 = sum . map (\x -> head . uncurry intersect $ splitAt (div (length x) 2) x)

q2 :: [[Int]] -> Int
q2 [] = 0
q2 input =
  let (cur, rest) = splitAt 3 input
   in (head . foldr1 intersect) cur + q2 rest

parse :: Text -> [Int]
parse = T.foldl (\xs x -> val x : xs) []

val :: Char -> Int
val c
  | isAsciiLower c = 1 + fromEnum c - fromEnum 'a'
  | isAsciiUpper c = 27 + fromEnum c - fromEnum 'A'
  | otherwise = 0
