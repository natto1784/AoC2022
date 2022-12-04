import Lib (split)

main :: IO ()
main = do
  input <- readFile "day4.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input

q1 :: String -> Int
q1 =
  length
    . filter
      ( \x ->
          let [[a, b], [c, d]] = map (map (read :: String -> Int) . split '-') $ split ',' x
           in a >= c && b <= d || c >= a && d <= b
      )
    . lines

q2 :: String -> Int
q2 =
  length
    . filter
      ( \x ->
          let [[a, b], [c, d]] = map (map (read :: String -> Int) . split '-') $ split ',' x
           in b >= c && a <= d || d >= a && c <= b
      )
    . lines
