-- we do not use a simple 9 value case cuz yeah

main :: IO ()
main = do
  choices <- parse <$> readFile "day2.in"
  putStr "Q1: "
  print $ q1 choices
  putStr "Q2: "
  print $ q2 choices

data Choice = Rock | Paper | Scissors deriving (Enum, Eq)

instance Ord Choice where
  compare x y | x == y = EQ
  compare Rock Paper = LT
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare _ _ = GT

parse :: String -> [(Choice, Choice)]
parse = map (\[x, _, y] -> (val x 'A', val y 'X')) . lines

score :: (Choice, Choice) -> Int
score (i, j) = 1 + fromEnum j + 3 * fromEnum (compare j i)

q1 :: [(Choice, Choice)] -> Int
q1 = sum . map score

q2 :: [(Choice, Choice)] -> Int
q2 = sum . map (score . \(i, j) -> (i, toEnum $ mod (fromEnum i + fromEnum j + 2) 3))

val :: Char -> Char -> Choice
val a b = toEnum (fromEnum a - fromEnum b) :: Choice
