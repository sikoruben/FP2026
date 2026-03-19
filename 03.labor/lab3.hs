import Control.Monad.Trans.Cont (reset)

-- # 3. labor

-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?

-- ```haskell
-- atlag :: (Floating a) => [a] -> a
-- atlag ls = (sum ls) / fromIntegral (length ls)

-- > (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > (take 4 . reverse . filter odd ) [1..20]
-- > take 4 . reverse . filter odd $ [1..20]
-- > take 4 ( reverse ( filter odd [1..20]))
-- > take 4 $ reverse $ filter odd $ [1..20]
-- ```

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely
-- - meghatározza egy lista elemszámát, 2 módszerrel (myLength),

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myLength2 :: [a] -> Int -> Int
myLength2 [] res = res
myLength2 (_ : xs) res = myLength2 xs (res + 1)

-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

-- - meghatározza egy lista legkisebb elemét (myMinimum),

myMinimum :: [Int] -> Int
myMinimum [x] = x
myMinimum (x : xs) = min x (myMinimum xs)

-- - meghatározza egy lista legnagyobb elemét (myMaximum),

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "Ures lista"
myMaximum [x] = x
myMaximum (x : xs) = max x (myMaximum xs)

-- - meghatározza egy lista n-ik elemét (!!),

getNth :: [a] -> Int -> a
getNth (x : _) 0 = x
getNth (_ : xs) n = getNth xs (n - 1)
getNth [] _ = error "Nagy index"

-- - egymásután fűzi a paraméterként megadott két listát (++),

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x : xs) ys = x : myAppend xs ys

-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- - meghatározza egy egész szám számjegyeinek listáját,

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse (helper (abs n))
  where
    helper 0 = []
    helper x = (x `mod` 10) : helper (x `div` 10)

-- - a lista első elemét elköltözteti a lista végére,

moveFirstToLast :: [a] -> [a]
moveFirstToLast [] = []
moveFirstToLast (x : xs) = xs ++ [x]

-- - meghatározza egy egész elemű lista elemeinek átlagértékét,

average :: [Int] -> Double
average [] = 0
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,

toBaseP :: Int -> Int -> [Int]
toBaseP 0 _ = [0]
toBaseP n p = reverse (helper n)
  where
    helper 0 = []
    helper x = (x `mod` p) : helper (x `div` p)

-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.

fromBaseP :: [Int] -> Int -> Int
fromBaseP ds p = foldl (\acc d -> acc * p + d) 0 ds

-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.

mapDigits :: [Int] -> [[Int]]
mapDigits ns = map digits ns

mapMoveFirst :: [[a]] -> [[a]]
mapMoveFirst lss = map moveFirstToLast lss

mapToBase2 :: [Int] -> [[Int]]
mapToBase2 ns = map (\n -> toBaseP n 2) ns

-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.

polyEval :: [Double] -> Double -> Double
polyEval coeffs x0 = foldr (\a acc -> a + x0 * acc) 0 coeffs

-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.

type Point = (Double, Double)

dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

closestPoint :: Point -> [Point] -> Point
closestPoint _ [] = error "Ures a pontok listaja"
closestPoint target lsP = foldl1 (\best p -> if dist target p < dist target best then p else best) lsP