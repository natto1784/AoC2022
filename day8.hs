import Data.Char (digitToInt)
import Data.List (transpose, zip4, zipWith4)

-- a fun approach, not necessarily the fastest (or cleanest (or smartest))

main :: IO ()
main = do
  input <- map (map digitToInt) . lines <$> readFile "day8.in"
  let (e, w, n, s) = trees input
  let obscured = zipWith4 zip4 e w n s
  putStr "Q1: "
  print $
    length $
      filter
        (not . (\((w, _), (x, _), (y, _), (z, _)) -> w && y && x && z))
        $ concat obscured
  putStr "Q2: "
  print $
    maximum $
      map (\((_, w), (_, x), (_, y), (_, z)) -> w * x * y * z) $
        concat obscured

type Forest = [[(Bool, Int)]]

trees :: [[Int]] -> (Forest, Forest, Forest, Forest)
trees input =
  ( trees' input,
    map reverse $ trees' $ map reverse input,
    transpose $ trees' $ transpose input,
    reverse $ transpose $ trees' $ transpose $ reverse input
  ) -- (east, west, north, south) traversals
  where
    trees' :: [[Int]] -> Forest
    trees' =
      foldr
        ( \x xs ->
            let (_, dist) =
                  foldr
                    ( \(cur, y) ((old, m), ys) ->
                        ( if y > m then (cur, y) else (old, m),
                          ( y <= m,
                            let s = length $ takeWhile (< y) $ reverse $ take cur x
                             in if s == cur then s else s + 1
                          ) :
                          ys
                        )
                    )
                    ((-1, -1), [])
                    (zip [0 ..] x)
             in dist : xs
        )
        []
