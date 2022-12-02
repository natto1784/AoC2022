import Lib (calories)

main :: IO ()
main = print . maximum . calories =<< readFile "input"
