import qualified Data.Either                   as E
import           Data.Function                 (on)
import qualified Data.List                     as L
import           Data.Ord                      (comparing)
import qualified Data.Time                     as T
import qualified Data.Time.LocalTime           as LT
import qualified Text.ParserCombinators.Parsec as P
import qualified System.IO                     as IO
import qualified System.IO.Unsafe              as IOU

data Input = Input { time :: LT.LocalTime, minutes :: Int, activity :: Either String Int }
  deriving (Show, Ord, Eq)

doParseTime = T.parseTimeM False T.defaultTimeLocale "%Y-%m-%d %H:%M"

parseTime :: P.GenParser Char st (LT.LocalTime, Int)
parseTime = do
  P.char '['
  timeStamp <- P.count 16 P.anyChar
  let minutes = read $ drop 14 timeStamp
  P.char ']'
  case doParseTime timeStamp of
    Nothing -> error $ "failed to parse timestamp " ++ timeStamp
    Just t -> return (t, minutes)

parseActivity :: P.GenParser Char st (Either String Int)
parseActivity = do
  activity <- (Left <$> (P.string "falls asleep" P.<|> P.string "wakes up")) P.<|> (Right <$> parseGuard)
  return activity

parseGuard :: P.GenParser Char st Int
parseGuard = do
  P.string "Guard #"
  guard <- read <$> P.many P.digit
  P.string " begins shift"
  return guard

parser :: P.GenParser Char st Input
parser = do
  (time, minutes) <- parseTime
  P.space
  activity <- parseActivity
  return $ Input time minutes activity

diff :: LT.LocalTime -> LT.LocalTime -> T.NominalDiffTime
diff st en =
  let tz = IOU.unsafePerformIO T.getCurrentTimeZone
   in T.diffUTCTime (LT.localTimeToUTC tz st) (LT.localTimeToUTC tz en)

toGuardTimes :: [Input] -> [(Int, [Int])]
toGuardTimes is = go is 0 0
  where
    go :: [Input] -> Int -> Int -> [(Int, [Int])]
    go []     _ _ = []
    go (i:is) g m
      = case activity i of
          Left "wakes up"     -> (g, [m..(minutes i)-1]) : go is g    (minutes i)
          Left "falls asleep" ->                           go is g    (minutes i)
          Right newG          ->                           go is newG (minutes i)

sumGuardTimes :: [(Int, [Int])] -> [(Int, [Int])]
sumGuardTimes = L.map (\l -> (fst l, concat $ snd l)) . L.map (\l -> (fst . head $ l, map snd l)) . L.groupBy ((==) `on` fst) . L.sortBy (comparing fst)

sleepiestGuard :: [(Int, [Int])] -> (Int, [Int])
sleepiestGuard = L.maximumBy (comparing $ length . snd)

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . L.group . L.sort

main = do
  inputLines <- lines <$> IO.readFile "input.txt"
  let inputs = L.sort $ E.rights $ map (P.parse parser "") inputLines
  let guardTimes = toGuardTimes inputs
  let totalTimes = sumGuardTimes guardTimes
  let target = sleepiestGuard totalTimes
  print $ (fst target) * (mostCommon $ snd target)
