-- we do not use a simple 9 value case cuz yeah

main :: IO ()
main = do
  input <- readFile "day2.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input

data Choice = Rock | Paper | Scissors deriving (Enum, Eq)

q1 :: String -> Int
q1 =
  sum
    . map
      ( \[x, _, y] ->
          let i = val x 'A'
              j = val y 'X'
           in fromEnum j + 1 + case () of
                _
                  | i == j -> 3
                  | winCond i == j -> 6
                  | otherwise -> 0
      )
    . lines

q2 :: String -> Int
q2 =
  sum
    . map
      ( \[x, _, y] ->
          let i = val x 'A'
              j = val y 'X'
           in 1 + case fromEnum j of
                0 -> 0 + fromEnum (loseCond i)
                1 -> 3 + fromEnum i
                2 -> 6 + fromEnum (winCond i)
      )
    . lines

val :: Char -> Char -> Choice
val a b = toEnum (fromEnum a - fromEnum b) :: Choice

winCond :: Choice -> Choice
winCond c = case c of
  Rock -> Paper
  Paper -> Scissors
  Scissors -> Rock

loseCond :: Choice -> Choice
loseCond c = case c of
  Paper -> Rock
  Scissors -> Paper
  Rock -> Scissors
