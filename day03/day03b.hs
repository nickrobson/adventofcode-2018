import qualified Data.Either                   as E
import qualified Text.ParserCombinators.Parsec as P
import qualified System.IO                     as IO
import Debug.Trace

data Input = Input { inputId :: String, coords :: (Int, Int), size :: (Int, Int) }
  deriving (Show, Eq)

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

intersects :: Input -> Input -> Bool
intersects a b
  = all not [minX a > maxX b, minX b > maxX a, minY a > maxY b, minY b > maxY a]
  where
    minX i = fst (coords i)
    maxX i = minX i + fst (size i) - 1
    minY i = snd (coords i)
    maxY i = minY i + snd (size i) - 1

findNoIntersects :: [Input] -> [Input] -> Maybe Input
findNoIntersects as [] = Nothing
findNoIntersects as (i:js)
 | hasIntersects i as = findNoIntersects as js
 | otherwise          = Just i

hasIntersects :: Input -> [Input] -> Bool
hasIntersects i js = any id $ map (\j -> i /= j && intersects i j) js

main = do
  inputLines <- lines <$> IO.readFile "input.txt"
  let inputs = E.rights $ map (P.parse parser "") inputLines
  case findNoIntersects inputs inputs of
    Just i  -> print i
    Nothing -> error "no inputs with no intersects found"
