{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Lib (readFile')

main :: IO ()
main = do
  input <- break (== T.empty) . T.lines <$> readFile' "day5.in"
  let (crates, cmds) = parse input
  putStr "Q1: "
  print $ moveCrates T.reverse crates cmds
  putStr "Q2: "
  print $ moveCrates id crates cmds

moveCrates :: (Text -> Text) -> [Text] -> [(Int, Int, Int)] -> Text
moveCrates f crates' cmds = head . T.transpose . M.elems $ foldl' move crates cmds
  where
    crates :: Map Int Text
    crates = M.fromList $ zip [0 ..] crates'
    move :: Map Int Text -> (Int, Int, Int) -> Map Int Text
    move c (n, x, y) =
      let (h, t) = T.splitAt n $ c M.! x
       in M.update (Just . (<>) (f h)) y $ M.insert x t c

-- ugly code vs line length <= 90 🤔
parse :: ([Text], [Text]) -> ([Text], [(Int, Int, Int)])
parse (crates, cmds) =
  ( [ T.filter (/= ' ') x
      | (i, x) <- zip [0 ..] (T.transpose $ init crates),
        i `mod` 4 == 1
    ],
    map
      ( ( \["move", T.unpack -> n, "from", T.unpack -> x, "to", T.unpack -> y] ->
            (read n, read x - 1, read y - 1)
        )
          . T.words
      )
      $ tail cmds
  )
