
askQuestion :: Int -> IO Bool
askQuestion mid = do
    putStrLn ("DEBUG: checking border " ++ show mid)
    putStrLn ("The number is smaller or equal " ++ show mid ++ "? (Yes/No)")
    answer <- getLine
    if answer == "Yes" || answer == "yes"
        then return True
        else if answer == "No" || answer == "no"
            then return False
            else do
                putStrLn "Error! enter Yes or No."
                askQuestion mid

guess :: Int -> Int -> IO ()
guess low high
    | low == high = do
        putStrLn ("Number is: " ++ show low)
    | otherwise = do
        let mid = (low + high) `div` 2
        putStrLn ("DEBUG: range from " ++ show low ++ " to " ++ show high)
        answer <- askQuestion mid
        if answer
            then guess low mid
            else guess (mid + 1) high

main :: IO ()
main = do
    putStrLn "think of a number from 0 to 99."
    guess 0 99