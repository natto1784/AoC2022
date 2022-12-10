{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Text (Text)
import qualified Data.Text as T (lines, pack, unlines, words)
import qualified Data.Text.IO as T (putStr)
import qualified Data.Text.Read as T (decimal, signed)
import Lib (readFile')

main :: IO ()
main = do
  vals <-
    scanl1 (+)
      . reverse
      . foldl
        ( \xs x ->
            let o : n = T.words x
                dec (T.signed T.decimal . head -> Right (m, "")) = m
             in if o == "noop" then 0 : xs else dec n : 0 : xs
        )
        [1]
      . T.lines
      <$> readFile' "day10.in"
  putStr "Q1: "
  print $ q1 vals
  putStrLn "Q2: "
  T.putStr $ q2 vals

q1 :: [Int] -> Int
q1 v = foldr (\x xs -> xs + (v !! (x - 1) * x)) 0 [20, 60 .. 220]

q2 :: [Int] -> Text
q2 v =
  T.unlines $
    map
      ( \x ->
          T.pack $
            map (\y -> if abs (v !! (x + y) - y) <= 1 then '#' else '.') [0 .. 39]
      )
      [0, 40 .. 200]
