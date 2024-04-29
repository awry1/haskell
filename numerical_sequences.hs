-- sortujCiag zwraca n ostatnich cyfr ciągu poprzez jego rekurencyjne dzielenie przez 10 i zapisywanie reszty
sortujCiag:: Integer -> Integer -> String -> String
sortujCiag number n word
  | n == 0 = word
  | otherwise = sortujCiag (number `div` 10) (n - 1) (word ++ show (number `mod` 10))

-- sortujStart wywołuje sortujCiag z początkową wartością word = ""
sortujStart:: Integer -> Integer -> String
sortujStart number n = sortujCiag number n ""

-- powtarzaj zwraca ciąg powstały z powtórzenia liczby m p razy
powtarzaj:: Integer -> Integer -> String 
powtarzaj m p 
  | p == 0 = ""
  | otherwise = show m ++ powtarzaj m (p - 1)

-- stworzCiag rekurencyjnie wywołuje powtarzaj dla każdej liczby z zakresu 0 do m
stworzCiag:: Integer -> Integer -> String
stworzCiag m p
  | m == 0 = powtarzaj m p
  | otherwise = powtarzaj m p ++ stworzCiag (m - 1) p

-- leksyk jeśli dane są poprawne wywołuje sortujStart z przekazanym wynikiem stworzCiag
leksyk:: Integer -> Integer -> Integer -> String
leksyk n m p  
  | n > (m + 1) * p = "Blad"
  | otherwise = sortujStart (read (stworzCiag m p) :: Integer) n

-- Przykładowe użycie
main :: IO ()
main = do
  print (leksyk 4 4 1)	-- 0123
  print (leksyk 4 2 2)	-- 0011
  print (leksyk 3 2 2)	-- 001