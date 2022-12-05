import Lib (split)

main :: IO ()
main = do
  input <- readFile "day5.in"
  let [crates, cmds] = split "" $ lines input
  let cratesList = init $ parseCrates $ init crates
  let cmdsList = parseCmds cmds
  putStr "Q1: "
  print $ q1 cratesList cmdsList
  putStr "Q2: "
  print $ q2 cratesList cmdsList

q1 :: [[Char]] -> [(Int, Int, Int)] -> [Char]
q1 crates cmds = map head $ foldl (\x xs -> moveCrates x xs True) crates cmds

q2 :: [[Char]] -> [(Int, Int, Int)] -> [Char]
q2 crates cmds = map head $ foldl (\x xs -> moveCrates x xs False) crates cmds

-- Computers are fast, really, not optimising right now
moveCrates :: [[Char]] -> (Int, Int, Int) -> Bool -> [[Char]]
moveCrates crates (n, a, b) rev =
  let (head, tail) = splitAt n $ crates !! a
   in replace b (replace a crates tail) ((if rev then reverse head else head) ++ crates !! b)
  where
    replace :: Int -> [a] -> a -> [a]
    replace i xs x = take i xs ++ [x] ++ drop (i + 1) xs

parseCrates :: [[Char]] -> [[Char]]
parseCrates ([] : _) = [[]]
parseCrates crates = parseCrate crates : parseCrates (map (drop 4) crates)
  where
    parseCrate :: [[Char]] -> [Char]
    parseCrate = foldr (\(_ : x : _ : _) xs -> if x /= ' ' then x : xs else xs) []

parseCmds :: [[Char]] -> [(Int, Int, Int)]
parseCmds = foldr (\x xs -> let y = split ' ' x in (read $ y !! 1, read (y !! 3) - 1, read (y !! 5) - 1) : xs) []
