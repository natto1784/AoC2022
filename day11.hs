{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Either (rights)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M (elems, fromList, insert, keys, lookup, update)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))
import Data.Text (Text)
import qualified Data.Text as T (empty, init, lines, stripSuffix, words)
import qualified Data.Text.Read as T (decimal, signed)
import Lib (readFile')
import Prelude hiding (round)

-- again a slow program ;-; takes 410 ms when measured with `time`

main :: IO ()
main = do
  input <- parse . T.lines <$> readFile' "day11.in"
  putStr "Q1: "
  print $ q 3 20 input
  putStr "Q2: "
  print $ q 1 10000 input

data Monkey = Banana
  { items :: [Int],
    operation :: Int -> Int -> Int,
    factor :: Int,
    next :: (Int, Int),
    iCount :: Int
  }

q :: Int -> Int -> Map Int Monkey -> Int
q d n =
  product . take 2
    . sortOn Down
    . map iCount
    . M.elems
    . (!! n)
    . iterate (round d)

round :: Int -> Map Int Monkey -> Map Int Monkey
round d monkeys =
  foldl
    ( \m k ->
        let (Banana items op f (a, b) c) = fromJust $ M.lookup k m
         in M.insert k (Banana [] op f (a, b) (c + length items)) $
              foldr
                ( \x m ->
                    let worry = op x d `mod` divisor
                     in M.update
                          ( Just
                              . ( \(Banana items' op' f' n' c') ->
                                    Banana (worry : items') op' f' n' c'
                                )
                          )
                          (if worry `mod` f == 0 then a else b)
                          m
                )
                m
                items
    )
    monkeys
    (M.keys monkeys)
  where
    divisor :: Int
    divisor = foldr1 lcm (map factor $ M.elems monkeys)

parse :: [Text] -> Map Int Monkey
parse =
  M.fromList
    . map
      ( \( (parseMonkey -> n)
             : (parseItems -> xs)
             : (parseOp -> op)
             : (parseTest -> (f, a, b))
           ) -> (n, Banana xs op f (a, b) 0)
      )
    . sLines
  where
    parseMonkey :: Text -> Int
    parseMonkey (T.words -> ["Monkey", T.decimal . T.init -> Right (n, "")]) = n

    parseItems :: Text -> [Int]
    parseItems (T.words -> "Starting" : "items:" : xs) =
      map (\(T.decimal -> Right (n, _)) -> n) xs

    parseOp :: Text -> (Int -> Int -> Int)
    parseOp (T.words -> ["Operation:", "new", "=", e1, o, e2]) =
      \x d -> div (parseO o (parseE x e1) (parseE x e2)) d
      where
        parseO o = if o == "+" then (+) else (*)
        parseE x e = case T.decimal e of
          Right (n, "") -> n
          Left _ -> x

    parseTest :: [Text] -> (Int, Int, Int)
    parseTest
      [ T.words -> ["Test:", "divisible", "by", T.decimal -> Right (t, "")],
        T.words -> ["If", "true:", "throw", "to", "monkey", T.decimal -> Right (a, "")],
        T.words -> ["If", "false:", "throw", "to", "monkey", T.decimal -> Right (b, "")]
        ] = (t, a, b)

    sLines :: [Text] -> [[Text]]
    sLines [] = []
    sLines input =
      let (cur, rest) = break (== T.empty) input
       in cur : sLines (drop 1 rest)
