import qualified Data.Map as M
import           System.IO

countChars :: String -> M.Map Char Int
countChars = foldr (\x m -> M.insertWith (+) x 1 m) M.empty

parseBoxId :: String -> (Bool, Bool)
parseBoxId s = do
  let charCounts = M.elems $ countChars s
  (elem 2 charCounts, elem 3 charCounts)

checksum :: [String] -> (Int, Int) -> (Int,Int)
checksum (x:xs) (d,t) = do
  let (a,b) = parseBoxId x
  checksum xs (if a then d + 1 else d, if b then t + 1 else t)
checksum [] c = c

main = do
  input <- lines <$> readFile "input.txt"
  let (d, t) = checksum input (0,0)
  print (d*t)