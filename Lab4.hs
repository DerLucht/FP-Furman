-- curried - функція, яка логічно приймає кілька аргументів, фактично приймає один аргумент і повертає іншу функцію, яка очікує наступний аргумент.

-- Часткове застосування — це передавання функції лише частини її аргументів, щоб отримати нову функцію з меншою кількістю аргументів.


productFive :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
productFive a b c d e = a * b * c * d * e

productOne :: Integer -> Integer -> Integer -> Integer -> Integer
productOne = productFive 2

productTwo :: Integer -> Integer -> Integer -> Integer
productTwo = productFive 2 3

productThree :: Integer -> Integer -> Integer
productThree = productFive 2 3 4

productFour :: Integer -> Integer
productFour = productFive 2 3 4 5

main :: IO ()
main = do
    putStrLn "Dobutok pyaty chysel:"
    print (productFive 2 3 4 5 6)

    putStrLn "Partial application to 1 argument:"
    print (productOne 3 4 5 6)

    putStrLn "Partial application to 2 arguments:"
    print (productTwo 4 5 6)

    putStrLn "Partial application to 3 arguments:"
    print (productThree 5 6)

    putStrLn "Partial application to 4 arguments:"
    print (productFour 6)