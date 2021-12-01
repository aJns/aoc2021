import System.Environment

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
    let numberList = createSums [] $ map (read::String->Int) cLines

    let first = head numberList
    let count = checkDepth (tail numberList) first 0
    putStrLn $ show count


checkDepth :: [Int] -> Int -> Int -> Int
checkDepth [] prev count = count
checkDepth aList prev count = checkDepth (tail aList) next newCount
    where next = head aList
          newCount = if prev < next then count + 1
                                    else count

createSums :: [Int] -> [Int] -> [Int]
createSums sumList (a:b:c:xs) = createSums (sumList ++ [newSum]) ([b] ++ [c] ++ xs)
    where newSum = a + b + c
createSums sumList _ = sumList
