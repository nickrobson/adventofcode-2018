import System.IO

doRead :: String -> Int
doRead ('+':s) = doRead s
doRead s = read s

main = do
  input <- readFile "input.txt"
  print $ sum . map doRead $ lines input