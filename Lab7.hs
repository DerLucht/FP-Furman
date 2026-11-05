
-- Представлення лямбда-виразів
data Expr = Var String
          | Lam String Expr
          | App Expr Expr
          deriving (Eq)

-- Перетворення виразу у текстовий вигляд
showExpr :: Expr -> String
showExpr (Var x) = x
showExpr (Lam x body) =
    "(\\" ++ x ++ "." ++ showExpr body ++ ")"
showExpr (App f a) =
    "(" ++ showExpr f ++ " " ++ showExpr a ++ ")"

-- Заміна змінної у виразі
subst :: String -> Expr -> Expr -> Expr
subst x replacement (Var y)
    | x == y = replacement
    | otherwise = Var y

subst x replacement (App f a) =
    App (subst x replacement f)
        (subst x replacement a)

subst x replacement (Lam y body)
    | x == y = Lam y body
    | otherwise = Lam y (subst x replacement body)

-- Виконання одного кроку бета-редукції
step :: Expr -> Maybe Expr

step (App (Lam x body) arg) =
    Just (subst x arg body)

step (App f a) =
    case step f of
        Just f' -> Just (App f' a)
        Nothing ->
            case step a of
                Just a' -> Just (App f a')
                Nothing -> Nothing

step (Lam x body) =
    case step body of
        Just body' -> Just (Lam x body')
        Nothing -> Nothing

step _ = Nothing

-- Виведення послідовності редукцій
printSteps :: Int -> Expr -> IO ()
printSteps 0 expr =
    putStrLn (showExpr expr)

printSteps n expr = do
    putStrLn (showExpr expr)
    case step expr of
        Just next ->
            printSteps (n - 1) next
        Nothing ->
            putStrLn "Редукцію завершено."

-- Визначення Y-комбінатора Каррі
yCombinator :: Expr
yCombinator =
    Lam "f" (App a a)
    where
        a = Lam "x"
            (App (Var "f")
                 (App (Var "x") (Var "x")))

-- Застосування Y-комбінатора до f
main :: IO ()
main = do
    putStrLn "Y-combinator:"
    putStrLn (showExpr yCombinator)

    putStrLn "\nBeta-reduction of Y f:"
    printSteps 3 (App yCombinator (Var "f"))
