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
myLength (_:xs) = 1 + myLength xs

myLength2 :: [a] -> Int -> Int
myLength2 [] res = res
myLength2 (_:xs) res = myLength2 xs (res + 1)

-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

-- - meghatározza egy lista legkisebb elemét (myMinimum),

myMinimum :: [Int] -> Int
myMinimum [x] = x
myMinimum (x:xs) = min x (myMinimum xs)

-- - meghatározza egy lista legnagyobb elemét (myMaximum),
-- - meghatározza egy lista n-ik elemét (!!),
-- - egymásután fűzi a paraméterként megadott két listát (++),
-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,
-- - meghatározza egy egész szám számjegyeinek listáját,
-- - a lista első elemét elköltözteti a lista végére,
-- - meghatározza egy egész elemű lista elemeinek átlagértékét,
-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,
-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.

-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.

-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.

-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.
