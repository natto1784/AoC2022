import Data.Char (isDigit)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  input <- lines <$> readFile "day5.in"
  let (crates, cmds) = break null input
  let cratesList = map catMaybes $ transpose $ map (fst . last . readP_to_S parseCrates) $ init crates
  let cmdsList = map ((\(n, a, b) -> (n, a - 1, b - 1)) . fst . last . readP_to_S parseCmd) $ tail cmds
  print cratesList
  putStr "Q1: "
  print $ q1 cratesList cmdsList
  putStr "Q2: "
  print $ q2 cratesList cmdsList

q1, q2 :: [[Char]] -> [(Int, Int, Int)] -> [Char]
q1 crates cmds = map head $ foldl (\x xs -> moveCrates x xs True) crates cmds
q2 crates cmds = map head $ foldl (\x xs -> moveCrates x xs False) crates cmds

-- Computers are fast, really, not optimising right now
moveCrates :: [[Char]] -> (Int, Int, Int) -> Bool -> [[Char]]
moveCrates crates (n, a, b) rev =
  let (head, tail) = splitAt n $ crates !! a
   in replace b (replace a crates tail) ((if rev then reverse head else head) ++ crates !! b)
  where
    replace :: Int -> [a] -> a -> [a]
    replace i xs x = take i xs ++ [x] ++ drop (i + 1) xs

parseCrates :: ReadP [Maybe Char]
parseCrates = sepBy parseRow (char ' ')
  where
    parseRow :: ReadP (Maybe Char)
    parseRow = (Just <$> (char '[' *> get <* char ']')) +++ (Nothing <$ string "   ")

parseCmd :: ReadP (Int, Int, Int)
parseCmd =
  (,,) <$> (string "move " *> getInt)
    <*> (string " from " *> getInt)
    <*> (string " to " *> getInt)
  where
    getInt :: ReadP Int
    getInt = read <$> many1 (satisfy isDigit)
