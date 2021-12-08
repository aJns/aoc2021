import System.Environment
import Data.Strings

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
    let (input, output) = unzip $ map (strSplit " | ") cLines

    let outputDigits = concat $ map (parseDigits . (strSplitAll " ")) output

    putStrLn $ show $ length outputDigits


parseDigits :: [String] -> [Int]
parseDigits a = parseDigits' [] a

parseDigits' :: [Int] -> [String] -> [Int]
parseDigits' a [] = a
parseDigits' digitList (digit:dTail)
  | dLen == 2 = parseDigits' (1:digitList) dTail
  | dLen == 4 = parseDigits' (4:digitList) dTail
  | dLen == 3 = parseDigits' (7:digitList) dTail
  | dLen == 7 = parseDigits' (8:digitList) dTail
  | otherwise = parseDigits' digitList dTail
  where dLen = length digit
