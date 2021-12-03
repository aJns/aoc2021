import System.Environment

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
