
-- швидко обчислювати результат певної операції над елементами масиву для різних діапазонів.


-- 1. Структура дерева відрізків

data Tree a
    = Leaf Int a
    | Node Int Int (Tree a) (Tree a) a


-- лист дерева, який містить індекс елемента та його значення. Leaf 2 7 елемент з індексом 2 має значення 7.


-- 2. Отримання результату, збереженого у вузлі

getValue :: Tree a -> a

-- Якщо це лист, поверт значення елемента
getValue (Leaf _ x) = x

-- Якщо це вузол, повертаємо результат операції, який був обчислений раніше.
getValue (Node _ _ _ _ x) = x


-- 3. Побудова дерева відрізків

buildTree :: (a -> a -> a) -> [a] -> Tree a

buildTree op xs
    | null xs = error "Масив не повинен бути порожнім"
    | otherwise = build 0 (length xs - 1)

    where
        build l r
            | l == r = Leaf l (xs !! l)

            | otherwise =
                Node l r left right (op leftValue rightValue)

                where
                    middle = (l + r) `div` 2

                    left = build l middle
                    right = build (middle + 1) r

                    leftValue = getValue left
                    rightValue = getValue right


-- запит до дерева

query :: (a -> a -> a) -> Tree a -> Int -> Int -> Maybe a

query op (Leaf position value) l r
    | position >= l && position < r = Just value
    | otherwise = Nothing

query op (Node nodeLeft nodeRight left right value) l r
    | l <= nodeLeft && nodeRight < r =
        Just value

    | nodeRight < l || nodeLeft >= r =
        Nothing

    | otherwise =
        combine
            (query op left l r)
            (query op right l r)

    where
        combine Nothing y = y
        combine x Nothing = x
        combine (Just x) (Just y) = Just (op x y)


-- обробка одного діапазону

rangeQuery :: (a -> a -> a) -> Tree a -> Int -> Int -> a

rangeQuery op tree l r =
    case query op tree l r of
        Just result -> result
        Nothing -> error "Порожній або некоректний діапазон"


-- обробка всіх діапазонів

solve :: (a -> a -> a) -> [a] -> [(Int, Int)] -> [a]

solve op arr ranges
    | null arr = error "Масив не повинен бути порожнім"
    | otherwise = map process ranges

    where
        tree = buildTree op arr

        process (l, r)
            | l < 0 || r > length arr || l >= r =
                error "Некоректні межі діапазону"

            | otherwise =
                rangeQuery op tree l r


-- Вхідні дані для перевірки

arr :: [Int]
arr = [1, 0, 7, 8, 1]

ranges :: [(Int, Int)]
ranges = [(0, 3), (1, 4), (2, 5)]

op :: Int -> Int -> Int
op a b = a + b




main =
    do
        putStrLn "Array:"
        print arr

        putStrLn "Ranges:"
        print ranges

        putStrLn "Result:"
        print (solve op arr ranges)