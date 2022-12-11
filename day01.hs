import Data.Either (isLeft, rights)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Text (Text)
import qualified Data.Text as T (lines)
import qualified Data.Text.Read as T (decimal)
import Lib (readFile')

main :: IO ()
main = do
  input <- map T.decimal . T.lines <$> readFile' "day1.in"
  putStr "Q1: "
  print . maximum $ calories input
  putStr "Q2: "
  print . sum . take 3 . sortOn Down $ calories input

calories :: [Either String (Int, Text)] -> [Int]
calories [] = []
calories input =
  let (cur, rest) = break isLeft input
   in sum (map fst $ rights cur) : calories (drop 1 rest)
