import Control.Monad.Trans.Cont (reset)
-- # 2. labor

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szJszorzat 0 = 1
szJszorzat n = mod n 10 * szJszorzat (div n 10) 

szJszorzat2 n 
    | n < 0 = szJszorzat2 (abs n)
    | div n 10 == 0 = mod n 10
    | otherwise = mod n 10 * szJszorzat2 (div n 10)

-- - egy szám számjegyeinek összegét (2 módszerrel),

szJosszeg n
    | n < 0 = szJosszeg(abs n)
    | div n 10 == 0 = mod n 10
    | otherwise = mod n 10 + szJosszeg (div n 10)

szJosszeg2 n res
    | n < 0 = szJosszeg2 (abs n) res
    | n < 10 = res + n
    | otherwise = szJosszeg2 (div n 10) (res + mod n 10)

-- - egy szám számjegyeinek számát (2 módszerrel),

szJegySzam :: Int -> Int
szJegySzam n
    | abs n < 10 = 1
    | otherwise  = 1 + szJegySzam (div n 10)

szJegySzam2 n res
    | abs n < 10 = res + 1
    | otherwise  = szJegySzam2 (div n 10) (res + 1)

-- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:

--   ```haskell
--   > fugv4 577723707 7
--   35
--   ```

fugv4 :: Int -> Int -> Int
fugv4 0 _ = 0
fugv4 n d
    | mod n 10 == d = d + fugv4 (div n 10) d
    | otherwise     = fugv4 (div n 10) d

-- - egy szám páros számjegyeinek számát,

parosSzamjegyek :: Int -> Int
parosSzamjegyek 0 = 0
parosSzamjegyek n
    | even (mod n 10) = 1 + parosSzamjegyek (div n 10)
    | otherwise = parosSzamjegyek(div n 10)

-- - egy szám legnagyobb számjegyét,

legnagyobbJegy :: Int -> Int
legnagyobbJegy n 
    | abs n < 10 = abs n
    | otherwise = max ( mod ( abs n ) 10 ) (legnagyobbJegy ( div ( abs n) 10 ))

-- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
--   Példák függvényhívásokra:

--   ```haskell
--   fugv 7673573 10 7 -> 3
--   fugv 1024 2 1 -> 1
--   fugv 1023 2 1 -> 10
--   fugv 345281 16 4 -> 2
--   ```

fugv :: Int -> Int -> Int -> Int
fugv 0 _ _ = 0
fugv n b d
    | mod n b == d = 1 + fugv (div n b) b d
    | otherwise = fugv (div n b) b d


-- - az 1000-ik Fibonacci számot.

fib :: Integer -> Integer
fib n = fibseged n 0 1
    where 
        fibseged 0 a _ = a 
        fibseged n a b = fibseged ( n - 1 ) b (a + b)   

-- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

-- **Megoldott feladatok:**

-- - Határozzuk meg egy szám számjegyeinek összegét:
--   I. módszer:

--   ```haskell
szOsszeg :: Int -> Int
szOsszeg 0 = 0
szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)

--   > szOsszeg 123
--   ```

--   II. módszer:

--   ```haskell
szOsszeg1 :: Int -> Int -> Int
szOsszeg1 0 t = t
szOsszeg1 x t = szOsszeg1 (x `div` 10) ( t + x `mod` 10 )

--   > szOsszeg1 123 0
--   ``` -->
