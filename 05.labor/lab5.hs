import Control.Monad.Trans.Cont (reset)
import Data.List
import Distribution.Simple.Setup (trueArg)
import System.Win32 (LOCALESIGNATURE (lsCsbDefault))

-- # 5. labor

-- I. Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.

mySplitAt n xs = (take n xs, drop n xs)

myNotElem e [] = True
myNotElem e (x : xs) = e /= x && myNotElem e xs

myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs

myRepeat x = x : myRepeat x

myReplicate n x
  | n <= 0 = []
  | otherwise = x : myReplicate (n - 1) x

myCycle [] = error "ures lista"
myCycle xs = xs ++ myCycle xs

myIterate f x = x : myIterate f (f x)

myAny p [] = False
myAny p (x : xs) = p x || myAny p xs

myAll p [] = True
myAll p (x : xs) = p x && myAll p xs

-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,

myLengthL ls = foldl op 0 ls
  where
    op res k = res + 1

myLengthR ls = foldr op 0 ls
  where
    op k res = res + 1

mySumL ls = foldl op 0 ls
  where
    op res k = res + k

mySumR ls = foldr op 0 ls
  where
    op k res = res + k

myElemL c ls = foldl (op c) False ls
  where
    op c res k
      | c == k = True
      | otherwise = res

myElemR c ls = foldr (op c) False ls
  where
    op c k res = (c == k) || res

myReverseL ls = foldl op [] ls
  where
    op res k = k : res

myReverseR ls = foldr op [] ls
  where
    -- op k res = res ++ [k]
    op k res = res <> [k]

myProductL ls = foldl op 1 ls
  where
    op res k = res * k

myProductR ls = foldl op 1 ls
  where
    op k res = res * k

myMaximumL ls = foldl op (head ls) ls
  where
    op res k
      | res > k = res
      | otherwise = k

myMaximumL1 ls = foldl1 op ls -- kezdoertek a lista elso eleme lesz automatikusan
  where
    op res k
      | res > k = res
      | otherwise = k

-- erre a muveletre nem alkalmas a fold
myIns x ls = foldr (op x) [] ls
  where
    op x k res
      | x > k = k : res
      | otherwise = x : k : res

ins :: (Ord a) => a -> [a] -> [a]
ins x [] = [x]
ins x (k : ve)
  | x > k = k : ins x ve
  | otherwise = x : k : ve

myAppend ls1 ls2 = foldr op ls2 ls1
  where
    op k res = k : res

myMap fg ls = foldr (op fg) [] ls
  where
    op fg k res = fg k : res

myFilter fg ls = foldr (op fg) [] ls
  where
    op fg k res
      | fg k == True = k : res
      | otherwise = res

-- - meghatározza egy lista pozitív elemeinek összegét,

mySumPos ls = foldl op 0 ls
  where
    op res k
      | k > 0 = res + k
      | otherwise = res

-- - egy lista páros elemeinek szorzatát,

myProduct ls = foldl op 1 ls
  where
    op res k
      -- \| mod k 2 == 0 = res * k
      | odd k = res * k
      | otherwise = res

-- - n-ig a négyzetszámokat.

negyzetszam n = foldr op [] [1 .. n]
  where
    op k res = k * k : res

-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$

polinom x0 ls = foldr (op x0) 0 ls
  where
    op x0 k res = k + x0 * res

mySum2 a b = foldr (+) 0 [a + 1 .. b - 1]

-- III.

-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe

minHosszuSzavak :: [String] -> [String]
minHosszuSzavak [] = []
minHosszuSzavak ls = filter (\s -> length s == minLen) ls
  where
    minLen = minimum (map length ls)

-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.

talalat :: (Eq a) => a -> [a] -> [Int]
talalat e ls = [i | (x, i) <- zip ls [0 ..], x == e]

-- foldrmegoldas
talalatFold :: (Eq a) => a -> [a] -> [Int]
talalatFold e ls = foldr op (const []) ls 0
  where
    op x f i
      | x == e = i : f (i + 1)
      | otherwise = f (i + 1)

--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.

osszegT :: [(String, Int)] -> Int
osszegT ls = foldl op 0 ls
  where
    op res (nev, ertek) = res + ertek

osszegT' ls = sum (map snd ls)

--   Például:

--   ```haskell
--   > ls = [("golya",120),("fecske",85),("cinege",132)]
--   > osszegT ls
--   337
--   ```
-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

atlagTu :: [(String, [Double])] -> [String]
atlagTu ls = map format ls
  where
    format (nev, pontok) = nev ++ " " ++ show (sum pontok / fromIntegral (length pontok))

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```
