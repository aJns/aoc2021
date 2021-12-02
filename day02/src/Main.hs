import System.Environment
import Data.Strings

type Coords = (Int, Int, Int)


main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
    let commands = map parseCommand $ map (strSplit " ") cLines

    putStrLn $ show $ (\(a,b) -> a*b) $ execCommands (0,0,0) commands


parseCommand :: (String, String) -> (String, Int)
parseCommand (a,b) = (a, newB)
    where newB = read b


execCommands :: Coords -> [(String, Int)] -> (Int, Int)
execCommands (a,b,c) [] = (a,b)
execCommands (x,y, aim) (cmd:rest) = execCommands(x', y', aim') rest
    where x' = if strEq (fst cmd) "forward" then x + (snd cmd)
                                            else x
          y'
              | strEq (fst cmd) "forward" = y + (aim*(snd cmd))
              | otherwise = y
          aim'
              | strEq (fst cmd) "down" = aim + (snd cmd)
              | strEq (fst cmd) "up" = aim - (snd cmd)
              | otherwise = aim

