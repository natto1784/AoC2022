{-# LANGUAGE OverloadedStrings #-}

import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as T (head, lines, words)
import qualified Data.Text.Read as T (decimal)
import Data.Tree
import Lib (readFile')

-- this solution assumes that empty directories can exist,
-- if you want to ignore that, there is a slightly different code in day7'.hs

-- needs cleaning and improvement, although i like the fact
-- that the entire FS can be printed as a tree

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

-- wrapper to create an empty tree
emptyFs :: Text -> Filesystem
emptyFs n = Node (n, 0) []

-- This function is to replace a filesystem by name
-- Used to replace empty directories with parsed directories in code
replaceFs :: Filesystem -> Text -> Filesystem -> Filesystem
replaceFs (Node n xs) name fs =
  let (h, t) = span ((/= name) . fst . rootLabel) xs
   in Node
        (fst n, snd n + snd (rootLabel fs))
        (h ++ [fs] ++ tail' t)

-- Sum of all filesystems in the list
-- Used to calculate sum of leaves in code
totalSize :: [Filesystem] -> Int
totalSize = sum . map (snd . rootLabel)

-- All directories/files are parsed here into leaves
ls :: [[Text]] -> [Filesystem]
ls =
  map
    ( \[a, b] ->
        if a == "dir"
          then Node (b, 0) []
          else Node (b, fst . head $ rights [T.decimal a]) []
    )

-- main function where stuff happens
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
        replaced = replaceFs fs name newFs -- replace empty directory
     in parse nextInput replaced
  where
    cur = head input
