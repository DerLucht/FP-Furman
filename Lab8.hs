
-- Рекурсивна функція знаходження НСД
gcdRec :: Int -> Int -> Int
gcdRec m 0 = m
gcdRec m n = gcdRec n (m `mod` n)

-- Представлення кроків редукції
gcdSteps :: Int -> Int -> IO ()
gcdSteps m 0 = do
    putStrLn ("gcd " ++ show m ++ " 0 = " ++ show m)

gcdSteps m n = do
    putStrLn ("gcd " ++ show m ++ " " ++ show n)
    putStrLn ("= gcd " ++ show n ++ " (" ++ show m
              ++ " mod " ++ show n ++ ")")
    gcdSteps n (m `mod` n)

-- Головна функція
main :: IO ()
main = do
    putStrLn "Finding GCD using recursion"
    putStrLn "Initial expression: gcd 9 10"
    putStrLn "\nReduction steps:"
    gcdSteps 9 10
    putStrLn "\nFinal result:"
    print (gcdRec 9 10)
