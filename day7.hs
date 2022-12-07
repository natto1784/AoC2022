import Data.Tree

-- this solution assumes that empty directories can exist,
-- if you want to ignore that, there is a slightly more compact code as day7'.hs
-- needs cleaning and improvement

main :: IO ()
main = do
  tree <- flip parse (emptyFs "/") . map words . lines <$> readFile "day7.in"
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

tail' :: [a] -> [a]
tail' = drop 1

emptyFs :: String -> Tree (String, Int)
emptyFs n = Node (n, 0) []

replaceFs :: Tree (String, Int) -> String -> Tree (String, Int) -> Tree (String, Int)
replaceFs (Node n xs) name fs =
  let (h, t) = span ((/= name) . fst . rootLabel) xs
   in Node
        (fst n, snd n + snd (rootLabel fs))
        (h ++ [fs] ++ tail' t)

totalSize :: [Tree (String, Int)] -> Int
totalSize = sum . map (snd . rootLabel)

ls :: [[String]] -> [Tree (String, Int)]
ls = map (\[a, b] -> if a == "dir" then Node (b, 0) [] else Node (b, read a) [])

parse :: [[String]] -> Tree (String, Int) -> Tree (String, Int)
parse input fs = snd $ parse' input fs

parse' :: [[String]] -> Tree (String, Int) -> ([[String]], Tree (String, Int))
parse' input fs
  | null input || cur !! 1 == "cd" && cur !! 2 == ".." = (tail' input, fs)
  | cur !! 1 == "ls" =
    let (children, rest) = span ((/= "$") . head) (tail' input)
        childFs = ls children
        name = fst $ rootLabel fs
     in parse' rest (Node (name, totalSize childFs) childFs)
  | otherwise =
    let name = cur !! 2
        (nextInput, newFs) = parse' (tail' input) (emptyFs name)
        replaced = replaceFs fs name newFs -- replace empty directory
     in parse' nextInput replaced
  where
    cur = head input
