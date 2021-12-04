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

    let (winningCard, drawn) = playBingo [] draws cards
    let unmarked = filter (\x -> not (elem x drawn)) $ concat winningCard
    let lastDrawn = head drawn

    putStrLn $ show $ (*) lastDrawn $ foldl (+) 0 unmarked


getLines :: String -> IO [String]
getLines filename = do
    content <- readFile filename
    return $ lines content

playBingo :: [Int] -> [Int] -> [Card] -> (Card, [Int])
playBingo drawn toDraw [lastCard] =
  if null winningCards then playBingo newDrawn (tail toDraw) [lastCard]
                       else (lastCard, newDrawn)
                         where
                           newDrawn = lastDrawn:drawn
                           winningCards = checkCardsForWins newDrawn [lastCard] []
                           lastDrawn = head toDraw
playBingo drawn toDraw cards = playBingo newDrawn (tail toDraw) cardsWithoutWin
  where
    newDrawn = lastDrawn:drawn
    winningCards = checkCardsForWins newDrawn cards []
    lastDrawn = head toDraw
    cardsWithoutWin = filter (\x -> not (elem x winningCards)) cards


checkCardsForWins :: [Int] -> [Card] -> [Card] -> [Card]
checkCardsForWins _ [] winningCards = winningCards
checkCardsForWins drawn cards winningCards =
    if gotBingo then checkCardsForWins drawn restCards (card:winningCards)
                else checkCardsForWins drawn restCards winningCards
                    where gotBingo = checkCardForWin drawn card
                          card = head cards
                          restCards = tail cards


checkCardForWin :: [Int] -> Card -> Bool
checkCardForWin drawn card = rowBingo || colBingo
  where rowBingo = checkRowsForWin drawn card
        colBingo = checkRowsForWin drawn $ transpose card


checkRowsForWin :: [Int] -> Card -> Bool
checkRowsForWin drawn [] = False
checkRowsForWin drawn card =
    if gotBingo then True
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
parseRow str = map read $ filter (\x -> not (strEq x "")) $ strSplitAll " " str


tailOrEmpty :: [a] -> [a]
tailOrEmpty [] = []
tailOrEmpty a = tail a
