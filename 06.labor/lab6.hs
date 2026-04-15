-- # 6. labor

import Data.List (maximumBy)
import Data.Ord (comparing)
import GHC.Base (VecElem (Int16ElemRep))
import GHC.TopHandler (topHandlerFastExit)

-- I. Írjunk egy-egy Haskell függvényt, amely beolvass a billentyűzetről két természetes számot és kiírja a képernyőre

-- - a két szám közötti számok összegét,
-- - a két szám közötti prímszámok összegét,
-- - a két szám közötti azon számokat, amelyeknek legtöbb valódi osztója van.

-- Segedfugvenyek
isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = null [x | x <- [2 .. floor (sqrt (fromIntegral n))], n `mod` x == 0]

valodiOsztokSzama :: Int -> Int
valodiOsztokSzama n = length [x | x <- [2 .. n - 1], n `mod` x == 0]

feladat1_1 :: IO ()
feladat1_1 = do
  putStrLn "Adj meg ket szamot: "
  line1 <- getLine
  line2 <- getLine
  let a = read line1 :: Int
  let b = read line2 :: Int
  let tartomany = [min a b .. max a b]

  -- 1
  putStrLn $ "Szamok osszege:" ++ show (sum tartomany)

  -- 2
  let primek = filter isPrime tartomany
  putStrLn $ "Primek osszege:" ++ show (sum primek)

  -- 3
  let maxOsztok = maximum [valodiOsztokSzama x | x <- tartomany]
  let legjobbak = [x | x <- tartomany, valodiOsztokSzama x == maxOsztok]
  putStrLn $ "Legtobb valodi osztoval biro szamok: " ++ show legjobbak

-- II. Írjunk egy-egy Haskell függvényt, amely beolvassa a billentyűzetről az n természetes számot és kiírja a képernyőre

-- - n-ig a Fibonacci számok listáját ($n > 50$), úgy hogy a számok közé szóközt ír,
-- - n-ig a prímszámok listáját, úgy hogy a számok közé szóközt ír,
-- - az n kettes számrendszerbeli alakját, úgy hogy minden negyedik bit után egy szóközt ír,
-- - az n 16-os számrendszerbeli alakját, úgy hogy minden két szimbólum után egy szóközt ír, illetve az a, b, c, d, e, f szimbólumokat használja a 10-nél nagyobb számjegyek kódolására,
-- - az n értékének megfelelően a következő sorokat:

-- segedfuggvenyek
toBin :: Int -> String
toBin 0 = "0"
toBin n = reverse (helper n)
  where
    helper 0 = ""
    helper x = show (x `mod` 2) ++ helper (x `div` 2)

toHex :: Int -> String
toHex 0 = "0"
toHex n = reverse (helper n)
  where
    helper 0 = "0"
    helper x = digits !! (x `mod` 16) : helper (x `div` 16)
    digits = "0123456789abcdef"

groupString :: Int -> String -> String
groupString n str = unwords (reverse (chunks (reverse str)))
  where
    chunks [] = []
    chunks s = take n s : chunks (drop n s)

feladat2 :: IO ()
feladat2 = do
  putStr "n = "
  n <- readLn :: IO Int

  let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
  putStrLn $ "Fibonacci: " ++ unwords (map show (takeWhile (<= n) fibs))

  -- bin
  putStrLn $ "Bináris: " ++ groupString 4 (toBin n)
  -- Hexa 2-esével tagolva
  putStrLn $ "Hexa: " ++ groupString 2 (toHex n)

  -- (0,n) párosok
  putStrLn "Párok:"
  mapM_ (\i -> putStrLn $ unwords [show (x, i - x) | x <- [0 .. i]]) [0 .. n]

--   ```
--   (0, 0)
--   (0, 1) (1, 0)
--   (0, 2) (1, 1) (2, 0)
--   ...
--   (0, n) (1, n-1), ..., (n, 0)
--   ```
-- - az n értékének megfelelően az összes természetes szám kettes számrendszerbeli alakját,
--   például: $$n = 6:\ 0,\ 1,\ 10,\ 11,\ 100,\ 101,\ 110$$.

-- III. Írjunk egy-egy Haskell függvényt, amely a billentyűzetről olvas be egész számokat egy listába, majd kiírja a képernyőre, hogy

-- - hányszor szerepel egy adott egész szám a listában,
-- - melyek azok az egész számok, amelyek kisebbek a listában megadott számok átlagértékénél,
-- - minden listabeli elem hányszor szerepel a listában, azaz készítsünk elem előfordulási statisztikát.

-- IV. Írjunk egy-egy Haskell függvényt, amely a billentyűzetről végjelig olvas be karakterláncokat, és

-- - meghatározza a legnagyobbat,
-- - meghatározza a legnagyobb elemek indexét,
-- - az adatok rendezett sorrendjét.
