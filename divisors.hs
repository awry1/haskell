-- dzielniki zwraca liczbe dzielników mniejszych od zadanej liczby (0 w przypadku liczby 1)
dzielniki :: Int -> Int
dzielniki liczba
  | liczba == 1 = 0
  | otherwise = ilosc
  where
    -- utworzenie listy dzielników mniejszych od pierwiastka liczby, zliczenie jej długości
	-- i ewentualnie zwiększenie o 1 gdy zadana liczba jest kwadratem innej liczby całkowitej
    ilosc = length [x | x <- [2..pierwiastek], checkDiv liczba x] * 2 + fromEnum (checkDiv liczba pierwiastek)
    pierwiastek = floor (sqrt (fromIntegral liczba))	-- policzenie pierwiastka całkowitego
    checkDiv a b = a `rem` b == 0			-- sprawdzanie czy liczby są podzielne

-- Przykładowe użycie
main :: IO ()
main = do
  print (dzielniki 72)	-- 11
  print (dzielniki 6)	-- 3
  print (dzielniki 1)	-- 0