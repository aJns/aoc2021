import System.Environment
import Data.Set (Set, insert)
import qualified Data.Set as Set
import Data.Strings
import Data.List

type Dot = (Int, Int)

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
    let dots = getDots Set.empty cLines
    let folds = reverse $ getFolds [] cLines
    let folded = foldAll [head folds] dots

    putStrLn $ show $ length folded


foldAll :: [String] -> Set Dot -> Set Dot
foldAll [] dots = dots
foldAll (fold:folds) dots = foldAll folds newDots
  where newDots
          | isYFold = Set.map (foldDotHorizontal foldInt) dots
          | otherwise = Set.map (foldDotVertical foldInt) dots
        foldInt = read $ snd splitted
        isYFold = "y" == fst splitted
        splitted = strSplit "=" fold


foldDotHorizontal :: Int -> Dot -> Dot
foldDotHorizontal foldY (x,y) 
  | foldY > y = (x,y)
  | foldY == y = (-1,-1) -- non-supported case
  | otherwise = (x,newY)
  where newY = foldY - distToFold
        distToFold = y - foldY


foldDotVertical :: Int -> Dot -> Dot
foldDotVertical foldX (x,y) 
  | foldX > x = (x,y)
  | foldX == x = (-1,-1) -- non-supported case
  | otherwise = (newX,y)
  where newX = foldX - distToFold
        distToFold = x - foldX


getDots :: Set Dot -> [String] -> Set Dot
getDots dots [] = dots
getDots dots (x:xs)
  | x == "" = dots
  | otherwise = getDots newDots xs
  where newDots = Set.insert dot dots
        dot = (i,j)
        i = read $ fst $ strSplit "," x
        j = read $ snd $ strSplit "," x


getFolds :: [String] -> [String] -> [String]
getFolds folds [] = folds
getFolds folds (x:xs)
  | head (strSplitAll " " x) == "fold" = getFolds newFolds xs
  | otherwise = getFolds folds xs
  where newFolds = fold:folds
        fold = snd $ strSplit "fold along " x


dotToChar :: Set Dot -> (Int, Int) -> Char
dotToChar dots xy
  | Set.member xy dots = '#'
  | otherwise = '.'


visualizeDots :: (Int, Int) -> Set Dot -> [String]
visualizeDots (rows,cols) dots = map (map (dotToChar dots)) $ coordMatrix
  where coordMatrix = [[(x,y) | x <- [0..rows]] | y <- [0..cols]]




























