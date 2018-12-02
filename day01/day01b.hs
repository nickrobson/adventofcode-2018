import qualified Data.Map as M
import           System.IO

runLoop :: [Int] -> M.Map Int Int -> Int -> Int
runLoop (x:xs) m i = do
  case M.lookup i m of
    -- this is 'Just 1' because by the time we get back here,
    -- we've seen i again (so it's actually the second time)
    Just 1  -> i
    Just n  -> runLoop xs (M.insert i (n + 1) m) (x + i)
    Nothing -> runLoop xs (M.insert i 1       m) (x + i)

doRead :: String -> Int
doRead ('+':s) = doRead s
doRead s = read s

main = do
  deltas <- lines <$> readFile "input.txt"
  print $ runLoop (concat . repeat . map doRead $ deltas) M.empty 0