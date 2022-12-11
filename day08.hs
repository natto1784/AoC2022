import Data.List (scanl, tails, transpose, zip4, zipWith4)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import Lib (readFile')

-- new approach was hinted by an anon but I cannot quite get it right
-- a fun approach, not necessarily the fastest (or cleanest (or smartest))

main :: IO ()
main = do
  input <- map T.unpack . T.lines <$> readFile' "day8.in"
  putStr "Q1: "
  print $ q1 input
  putStr "Q2: "
  print $ q2 input

layer :: [[Char]] -> (a -> a -> a) -> ([[Char]] -> [[a]]) -> [[a]]
layer input f f' =
  zipWith4 ( zipWith4 (\w x y z -> f w . f x $ f y z) )
    (f' e)
    (map reverse $ f' w)
    (transpose $ f' n)
    (reverse . transpose $ f' s)
  where
    (e, w, n, s) =
      ( input,
        map reverse input,
        transpose input,
        transpose $ reverse input
      ) -- (east, west, north, south]

q1 :: [[Char]] -> Int
q1 input = length . filter not . concat $ layer input (&&) q1'
  where
    q1' :: [[Char]] -> [[Bool]]
    q1' = map (\x -> zipWith (<=) x $ scanl max minBound x)

q2 :: [[Char]] -> Int
q2 input = maximum . concat $ layer input (*) q2'
  where
    q2' :: [[Char]] -> [[Int]]
    q2' =
      map
        ( \a ->
            [ let l = length (takeWhile (< x) xs) in (+ l) . fromEnum $ length xs /= l
              | (x : xs) <- tails a
            ]
        )
