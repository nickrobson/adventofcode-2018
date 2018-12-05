import qualified Data.Either                   as E
import qualified Text.ParserCombinators.Parsec as P
import qualified System.IO                     as IO
import Debug.Trace

data Input = Input { id :: String, coords :: (Int, Int), size :: (Int, Int) }
  deriving (Show)

parseId :: P.GenParser Char st String
parseId = P.char '#' >> P.many P.digit

parseCoords :: P.GenParser Char st (Int, Int)
parseCoords = do
  x <- read <$> P.many P.digit
  P.char ','
  y <- read <$> P.many P.digit
  return (x,y)

parseSize :: P.GenParser Char st (Int, Int)
parseSize = do
  x <- read <$> P.many P.digit
  P.char 'x'
  y <- read <$> P.many P.digit
  return (x,y)

parser :: P.GenParser Char st Input
parser = do
  id <- parseId
  P.string " @ "
  coords <- parseCoords
  P.string ": "
  size <- parseSize
  return $ Input id coords size

increment :: Int -> Int -> [[Int]] -> [[Int]]
increment _ _ [] = []
increment x y (col:cols)
 | x > 0  = col                   : increment (x-1) y cols
 | x == 0 = incrementColumn y col : cols
 where
  incrementColumn :: Int -> [Int] -> [Int]
  incrementColumn _ [] = []
  incrementColumn y (val:vals)
   | y > 0  = val   : incrementColumn (y-1) vals
   | y == 0 = val+1 : vals

incrementBox :: [(Int, Int)] -> [[Int]] -> [[Int]]
incrementBox ((x,y):coords) fabric = incrementBox coords $ increment x y fabric
incrementBox []             fabric = fabric

incrementBoxes :: [[(Int, Int)]] -> [[Int]] -> [[Int]]
incrementBoxes (box:boxes) fabric = incrementBox box $ incrementBoxes boxes fabric
incrementBoxes []          fabric = fabric

toBox :: Input -> [(Int, Int)]
toBox i = let xs = concat $ replicate (maxY-minY) [minX..maxX-1]
              ys = concat $ map (replicate (maxX-minX)) [minY..maxY-1]
           in zip xs ys
  where
    minX = fst (coords i)
    maxX = minX + fst (size i)
    minY = snd (coords i)
    maxY = minY + snd (size i)

maxSize :: Int
maxSize = 1200

makeFabric :: [[Int]]
makeFabric = replicate maxSize (replicate maxSize 0)

main = do
  inputLines <- lines <$> IO.readFile "input.txt"
  let inputs = map (P.parse parser "") inputLines
  let boxes = map toBox $ E.rights inputs
  let fabric = incrementBoxes boxes makeFabric
  print $ length $ filter (> 1) $ concat fabric
