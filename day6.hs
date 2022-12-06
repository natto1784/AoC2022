import Data.List (nub)
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  input <- readFile "day6.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input


q1, q2 :: String -> Int
q1 = length . parse 4
q2 = length . parse 14

parse :: Int -> String -> String
parse n a = parse' n (splitAt n a)

parse' :: Int -> (String, String) -> String
parse' n (a, b)
  | length (nub (drop (length a - n) a)) == n = a
  | otherwise =
    let (c, cs) = last $ readP_to_S get b
     in parse' n (a ++ [c], cs)