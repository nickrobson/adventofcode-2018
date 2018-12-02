import qualified Data.Map as M
import           System.IO

reverse' :: [a] -> [a]
reverse' xs = go xs []
  where
    go (x:xs) acc = go xs (x:acc)
    go []     acc = acc


getCommonCharacters :: String -> String -> String
getCommonCharacters a b = go a b ""
  where
    go (a:as) (b:bs) acc | a == b    = go as bs (a:acc)
                         | otherwise = go as bs acc
    go _      _      acc             = acc

findNearest :: [String] -> [String] -> String
findNearest (a:as) (b:bs) = do
  let commonCharacters = getCommonCharacters a b
  if length commonCharacters + 1 == length a
    then reverse' commonCharacters
    else findNearest (a:as) bs
findNearest (a:as) [] = findNearest as as
findNearest []     _  = error "no near strings found"

main = do
  input <- lines <$> readFile "input.txt"
  putStrLn $ findNearest input input