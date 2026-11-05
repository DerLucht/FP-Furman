-- синонім типу не створює принципово нового типу, а лише надає зручніше ім'я вже існуючому


type Age = Int

type PersonName = String

type Pair a = (a, a)

type Triple a = (a, a, a)

type Result a = Maybe a
-- спеціальний тип, який означає, що значення може бути, а може бути відсутнім

type NumberList a = [a]

personName :: PersonName
personName = "Olena"

personAge :: Age
personAge = 20

numbers :: NumberList Int
numbers = [10, 20, 30, 40, 50]

coordinates :: Pair Int
coordinates = (5, 8)

rgb :: Triple Int
rgb = (255, 128, 0)

findAge :: Maybe Age
findAge = Just 20

missingAge :: Result Age
missingAge = Nothing

main :: IO ()
main = do
    putStrLn "New type synonyms in Haskell"
    putStrLn ("Name: " ++ personName)
    putStrLn ("Age: " ++ show personAge)
    putStrLn ("Numbers: " ++ show numbers)
    putStrLn ("Coordinates: " ++ show coordinates)
    putStrLn ("RGB: " ++ show rgb)
    putStrLn ("Found age: " ++ show findAge)
    putStrLn ("Missing age: " ++ show missingAge)