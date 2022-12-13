{-# OPTIONS_GHC -Wno-missing-methods #-}

import Control.Monad (ap)
import Data.Map (Map)
import qualified Data.Map as M (filter, fromList, keys, (!), (!?))
import Data.Set (Set)
import qualified Data.Set as S (deleteAt, elemAt, empty, fromList, insert, member, null, singleton, union)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import Lib (readFile')

main :: IO ()
main = do
  input <- parse . T.lines <$> readFile' "day12.in"
  print $ head . M.keys $ M.filter (== S) input
  putStr "Q1: "
  print $ shortest (S.singleton (0, head . M.keys $ M.filter (== S) input)) input E
  putStr "Q2: "
  print $
    shortest
      ( S.fromList
          ( zip
              (repeat 0)
              (M.keys $ M.filter (\x -> x == S || x == height 'a') input)
          )
      )
      input
      E

data Height = Height Int | S | E deriving (Show, Eq)

-- no need to implement toEnum
instance Enum Height where
  fromEnum S = 0
  fromEnum E = 25
  fromEnum (Height h) = h

height :: Char -> Height
height 'E' = E
height 'S' = S
height c = Height $ fromEnum c - fromEnum 'a'

parse :: [Text] -> Map (Int, Int) Height
parse =
  M.fromList . concat
    . zipWith zip (map (\x -> zip (repeat x) [0 ..]) [0 ..])
    . map (map height . T.unpack)

nextPos :: (Int, Int) -> Map (Int, Int) Height -> [(Int, Int)]
nextPos (x, y) m = filter check [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    check :: (Int, Int) -> Bool
    check (x', y') =
      maybe
        False
        (<= (succ . fromEnum $ m M.! (x, y)))
        (fromEnum <$> m M.!? (x', y'))

-- using set as a queue cuz yh
shortest :: Set (Int, (Int, Int)) -> Map (Int, Int) Height -> Height -> Int
shortest queue graph end = bfs queue S.empty
  where
    bfs :: Set (Int, (Int, Int)) -> Set (Int, Int) -> Int
    bfs queue vis
      | graph M.! (x, y) == end = now
      | otherwise = bfs newqueue (S.insert (x, y) vis)
      where
        (now, (x, y)) = S.elemAt 0 queue
        newqueue =
          S.union
            (S.deleteAt 0 queue)
            ( S.fromList
                ( zip
                    (repeat (now + 1))
                    (filter (not . flip S.member vis) $ nextPos (x, y) graph)
                )
            )
