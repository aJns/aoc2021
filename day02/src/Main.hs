import System.Environment
import Data.Strings


main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
    let commands = map parseCommand $ map (strSplit " ") cLines

    putStrLn $ show $ (\(a,b) -> a*b) $ execCommands (0,0) commands


parseCommand :: (String, String) -> (String, Int)
parseCommand (a,b) = (a, newB)
    where newB = read b


execCommands :: (Int, Int) -> [(String, Int)] -> (Int, Int)
execCommands a [] = a
execCommands (x,y) (cmd:rest) = execCommands(x', y') rest
    where x' = if strEq (fst cmd) "forward" then x + (snd cmd)
                                            else x
          y'
              | strEq (fst cmd) "down" = y + (snd cmd)
              | strEq (fst cmd) "up" = y - (snd cmd)
              | otherwise = y

