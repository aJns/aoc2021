import System.Environment

type Card = [[Int]]

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
    let inputs = divideInput cLines

    let draws = parseDraws $ head inputs
    let cards = map parseCards $ tail inputs

    let (winningLine, lastDrawn) = playBingo [] draws cards

    putStrLn $ show $ lastDrawn * $ foldl (+) winningLine

-- params: drawn, toDraw, cards
-- returns: winning row/col, last drawn
playBingo :: [Int] -> [Int] -> [Card] -> ([Int], Int)
playBingo drawn toDraw cards = 
    if gotBingo then (winningLine, lastDrawn)
                else playBingo (newDrawn) (tail toDraw) cards
                    where
                        newDrawn = [lastDrawn] ++ drawn
                        (gotBingo, winningLine) = checkCardsForWin newDrawn cards
                        lastDrawn = head toDraw


checkCardsForWin :: [Int] -> [Card] -> ([Int], Bool)
checkCardsForWin [] _ = (False, [])
checkCardsForWin _ [] = (False, [])
checkCardsForWin drawn cards =
    if gotBingo then (winningLine, True)
                else checkCardsForWin drawn $ tail cards
                    where (winningLine, gotBingo) = checkCardForWin drawn $ head card


checkCardForWin :: [Int] -> Card -> ([Int], Bool)
checkCardForWin drawn card =
    if rowBingo then (rowWin, True)
                else if colBingo (colWin, True)
                else ([], False)
                    where (rowWin, rowBingo) = checkRowsForWin drawn card
                    where (colWin, colBingo) = checkRowsForWin drawn $ transpose card


checkRowsForWin :: [Int] -> Card -> ([Int], Bool)
checkRowsForWin drawn [] = ([], False)
checkRowsForWin drawn card =
    if gotBingo then (winLine, True)
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


divideInput :: [String] -> [[String]]


parseDraws :: String -> [Int]


parseCard :: [String] -> [Card]
