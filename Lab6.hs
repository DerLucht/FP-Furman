-- рекурсія без викликання свого імені
-- фікс_ована точка тип даних ф яка приймає щось одне і повертає щось такого ж типу, потім змінна х = ф і повертає х
fix :: (a -> a) -> a
fix f = let x = f x in x

--  обчислення факторіалу (традиційний запис)
factorialRec :: Int -> Int
factorialRec n = if n == 0 then 1 else n * factorialRec (n - 1)

-- Приклад 2: Нерекурсивна функція, еквівалентна рекурсивній, 
-- побудована за допомогою Y-комбінатора (fix)
factorialNonRec :: Int -> Int
factorialNonRec = fix (\factGen n -> if n == 0 then 1 else n * factGen (n - 1))



main :: IO ()
main = do
  print $ factorialNonRec 5  -- Очікується: 120