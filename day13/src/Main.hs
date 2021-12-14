import System.Environment
import Data.Set (Set, insert)
import qualified Data.Set as Set
import Data.Strings

type Dot = (Int, Int)

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
    let dots = getDots Set.empty cLines
    let foldY = getFoldY cLines
    let folded = Set.map (foldDotHorizontal foldY) dots

    putStrLn $ show $ length folded


foldDotHorizontal :: Int -> Dot -> Dot
foldDotHorizontal foldY (x,y) 
  | foldY > y = (x,y)
  | foldY == y = (-1,-1) -- non-supported case
  | otherwise = (x,newY)
  where newY = foldY - distToFold
        distToFold = y - foldY


getDots :: Set Dot -> [String] -> Set Dot
getDots dots [] = dots
getDots dots (x:xs)
  | x == "" = dots
  | otherwise = getDots newDots xs
  where newDots = insert dot dots
        dot = (i,j)
        i = read $ fst $ strSplit "," x
        j = read $ snd $ strSplit "," x


getFoldY :: [String] -> Int
getFoldY (x:xs)
  | head (strSplitAll " " x) == "fold" = read $ snd $ strSplit "=" x
  | otherwise = getFoldY xs
