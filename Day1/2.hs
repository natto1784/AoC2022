import Data.List (sortOn)
import Data.Ord (Down (Down))
import Lib (calories)

main :: IO ()
main = print . sum . take 3 . sortOn Down . calories =<< readFile "input"
