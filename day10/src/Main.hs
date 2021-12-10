import System.Environment

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content

    let scores = map (charToVal . checkParens []) cLines

    putStrLn $ show $ foldl (+) 0 scores


charToVal :: Char -> Int
charToVal char
  | char == ')' = 3
  | char == ']' = 57
  | char == '}' = 1197
  | char == '>' = 25137
  | otherwise = 0


checkParens :: String -> String -> Char
checkParens stack [] = ' '
checkParens stack (x:xs)
  | elem x "([{<" = checkParens (x:stack) xs
  | elem x ")]}>" = if hasMatch (head stack) x
                       then checkParens (tail stack) xs
                       else x
  | otherwise = checkParens stack xs


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
