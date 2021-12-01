import System.Environment

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
    let numberList = map (read::String->Int) cLines

    let first = head numberList
    let count = checkDepth (tail numberList) first 0
    putStrLn $ show count


checkDepth :: [Int] -> Int -> Int -> Int
checkDepth [] prev count = count
checkDepth aList prev count = checkDepth (tail aList) next newCount
    where next = head aList
          newCount = if prev < next then count + 1
                                    else count
