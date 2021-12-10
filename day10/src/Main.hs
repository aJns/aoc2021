import System.Environment
import Data.List

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content

    -- let scores = map (charToVal . checkParens []) cLines
    let completes = map (\(_,y) -> y) $ filter (\(x,_) -> x) $ map (checkParens []) cLines
    let scores = map (calcComplScore 0) completes
    let sorted = sort scores
    let middleIndex = div ((length sorted)-1) 2

    putStrLn $ show $ sorted !! middleIndex


charToVal :: Char -> Int
charToVal char
  | char == ')' = 3
  | char == ']' = 57
  | char == '}' = 1197
  | char == '>' = 25137
  | otherwise = 0


calcComplScore :: Int -> String -> Int
calcComplScore score [] = score
calcComplScore score (a:as)
  | a == ')' = calcComplScore ((score*5)+1) as
  | a == ']' = calcComplScore ((score*5)+2) as
  | a == '}' = calcComplScore ((score*5)+3) as
  | a == '>' = calcComplScore ((score*5)+4) as
  | otherwise = 0


checkParens :: String -> String -> (Bool, String)
checkParens stack [] = (True, stack)
checkParens stack (x:xs)
  | elem x "([{<" = checkParens ((getCloser x):stack) xs
  | elem x ")]}>" = if x == (head stack)
                       then checkParens (tail stack) xs
                       else (False, [x])
  | otherwise = checkParens stack xs


getCloser :: Char -> Char
getCloser opener
  | opener == '(' = ')'
  | opener == '[' = ']'
  | opener == '{' = '}'
  | opener == '<' = '>'
  | otherwise = ' '

hasMatch :: Char -> Char -> Bool
hasMatch opener closer
  | opener == '(' = closer == ')'
  | opener == '[' = closer == ']'
  | opener == '{' = closer == '}'
  | opener == '<' = closer == '>'
  | otherwise = False

-- checkParens :: String -> Char
-- checkParens (x:xs)
--   | x == '(' = calcRetVal ')' $ checkParens xs
--   | x == '[' = calcRetVal ']' $ checkParens xs
--   | x == '{' = calcRetVal '}' $ checkParens xs
--   | x == '<' = calcRetVal '>' $ checkParens xs
--   | x == ')' = ')'
--   | x == ']' = ']'
--   | x == '}' = '}'
--   | x == '>' = '>'
--   | otherwise = ' '


calcRetVal :: Char -> Char -> Char
calcRetVal a b
  | a == b = ' '
  | otherwise = b
