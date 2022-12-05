import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    many1,
    readP_to_S,
    satisfy,
  )

main :: IO ()
main = do
  input <- map (fst . last . readP_to_S parse) . lines <$> readFile "day4.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input

parse :: ReadP ((Int, Int), (Int, Int))
parse = do
  a <- readInt
  char '-'
  b <- readInt
  char ','
  c <- readInt
  char '-'
  d <- readInt
  return ((a, b), (c, d))
  where
    readInt :: ReadP Int
    readInt = read <$> many1 (satisfy isDigit)

q1, q2 :: [((Int, Int), (Int, Int))] -> Int
q1 = length . filter (\((a, b), (c, d)) -> a >= c && b <= d || c >= a && d <= b)
q2 = length . filter (\((a, b), (c, d)) -> b >= c && a <= d)
