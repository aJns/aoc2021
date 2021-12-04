import System.Environment
import Data.Strings
import Data.List

type Card = [[Int]]

main = do
    args <- getArgs
    let filename = head args
    cLines <- getLines filename
    let inputs = divideInput [] $ reverse cLines

    let draws = parseDraws $ head $ head inputs
    let cards = map parseCard $ tail inputs

    putStrLn $ show $ cards
    -- let (winningLine, lastDrawn) = playBingo [] draws cards

    -- putStrLn $ show $ (*) lastDrawn $ foldl (+) 0 winningLine


getLines :: String -> IO [String]
getLines filename = do
    content <- readFile filename
    return $ lines content

-- params: drawn, toDraw, cards
-- returns: winning row/col, last drawn
playBingo :: [Int] -> [Int] -> [Card] -> ([Int], Int)
playBingo drawn toDraw cards = 
    if gotBingo then (winningLine, lastDrawn)
                else playBingo (newDrawn) (tail toDraw) cards
                    where
                        newDrawn = [lastDrawn] ++ drawn
                        (winningLine, gotBingo) = checkCardsForWin newDrawn cards
                        lastDrawn = head toDraw


checkCardsForWin :: [Int] -> [Card] -> ([Int], Bool)
checkCardsForWin [] _ = ([], False)
checkCardsForWin _ [] = ([], False)
checkCardsForWin drawn cards =
    if gotBingo then (winningLine, True)
                else checkCardsForWin drawn $ tail cards
                    where (winningLine, gotBingo) = checkCardForWin drawn $ head cards


checkCardForWin :: [Int] -> Card -> ([Int], Bool)
checkCardForWin drawn card =
    if rowBingo then (rowWin, True)
                else if colBingo then (colWin, True)
                else ([], False)
                    where (rowWin, rowBingo) = checkRowsForWin drawn card
                          (colWin, colBingo) = checkRowsForWin drawn $ transpose card


checkRowsForWin :: [Int] -> Card -> ([Int], Bool)
checkRowsForWin drawn [] = ([], False)
checkRowsForWin drawn card =
    if gotBingo then (row, True)
                else checkRowsForWin drawn $ tail card
                    where gotBingo = checkRowForWin drawn row
                          row = head card


checkRowForWin :: [Int] -> [Int] -> Bool
checkRowForWin _ [] = True
checkRowForWin [] _ = False
checkRowForWin drawn row =
    if wasDrawn then checkRowForWin drawn (tail row)
                else False
                    where wasDrawn = (head row) `elem` drawn


divideInput :: [[String]] -> [String] -> [[String]]
divideInput a [] = a
divideInput divided cLines
  | strEq current "" = divideInput ([]:divided) $ tail cLines
  | otherwise = divideInput (newHead:(tailOrEmpty divided)) $ tail cLines
  where current = head cLines
        newHead = if null divided  then [current]
                                   else current:(head divided)


parseDraws :: String -> [Int]
parseDraws str = map read $ strSplitAll "," str


parseCard :: [String] -> Card
parseCard strList = map parseRow strList


parseRow :: String -> [Int]
parseRow str = map read $ strSplitAll " " str


tailOrEmpty :: [a] -> [a]
tailOrEmpty [] = []
tailOrEmpty a = tail a
