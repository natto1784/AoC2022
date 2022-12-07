import Data.Tree

main :: IO ()
main = do
  tree <- snd . flip parse (emptyFs "/") . map words . lines <$> readFile "day7.in"
  putStr "Q1: "
  print $
    foldTree
      ( \x xs ->
          if null xs || snd x > 100000 then sum xs else snd x + sum xs
      )
      tree
  putStr "Q2: "
  let maxStorage = 70000000
  let spaceNeeded = 30000000 - (maxStorage - snd (rootLabel tree))
  print $
    foldTree
      ( \x xs ->
          if snd x < spaceNeeded
            then minimum (maxStorage : xs)
            else minimum (snd x : xs)
      )
      tree

type Filesystem = Tree (String, Int)

tail' :: [a] -> [a]
tail' = drop 1

emptyFs :: String -> Filesystem
emptyFs n = Node (n, 0) []

insertFs :: Filesystem -> String -> Filesystem -> Filesystem
insertFs (Node n xs) name fs = Node (fst n, snd n + snd (rootLabel fs)) (fs : xs)

totalSize :: [Filesystem] -> Int
totalSize = sum . map (snd . rootLabel)

ls :: [[String]] -> [Filesystem]
ls = map (\[a, b] -> Node (b, read a) []) . filter ((/= "dir") . head)

parse :: [[String]] -> Filesystem -> ([[String]], Filesystem)
parse input fs
  | null input || cur == ["$", "cd", ".."] = (tail' input, fs)
  | cur == ["$", "ls"] =
    let (children, rest) = span ((/= "$") . head) (tail' input)
        leaves = ls children
        name = fst $ rootLabel fs
     in parse rest (Node (name, totalSize leaves) leaves)
  | otherwise =
    let name = cur !! 2
        (nextInput, newFs) = parse (tail' input) (emptyFs name)
        replaced = insertFs fs name newFs -- replace empty directory
     in parse nextInput replaced
  where
    cur = head input
