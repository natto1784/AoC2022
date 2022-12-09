{-# LANGUAGE OverloadedStrings #-}

import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as T (head, lines, words)
import qualified Data.Text.Read as T (decimal)
import Data.Tree
import Lib (readFile')

main :: IO ()
main = do
  tree <- snd . flip parse (emptyFs "/") . map T.words . T.lines <$> readFile' "day7.in"
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

type Filesystem = Tree (Text, Int)

tail' :: [a] -> [a]
tail' = drop 1

emptyFs :: Text -> Filesystem
emptyFs n = Node (n, 0) []

insertFs :: Filesystem -> Text -> Filesystem -> Filesystem
insertFs (Node n xs) name fs = Node (fst n, snd n + snd (rootLabel fs)) (fs : xs)

ls :: [[Text]] -> [Filesystem]
ls =
  map (\[a, b] -> Node (b, fst . head $ rights [T.decimal a]) [])
    . filter ((/= "dir") . head)

totalSize :: [Filesystem] -> Int
totalSize = sum . map (snd . rootLabel)

parse :: [[Text]] -> Filesystem -> ([[Text]], Filesystem)
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
        replaced = insertFs fs name newFs
     in parse nextInput replaced
  where
    cur = head input
