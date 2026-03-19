import System.Win32 (LOCALESIGNATURE (lsCsbDefault), eNABLE_ECHO_INPUT)

-- # 4. labor

-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- - az első n páros szám négyzetét,

elsoNParosNegyzet n = [(2 * x) ^ 2 | x <- [0 .. n - 1]]

-- - az első $$[1, 2, 2, 3, 3, 3, 4, 4, 4, 4,\ldots]$$,

szamokLs n
  | n /= 1 = szamokLs (n - 1) ++ replicate n n
  | otherwise = replicate n n

szamokLs2 n i
  | i /= n = replicate i i ++ szamokLs2 n (i + 1)
  | otherwise = replicate i i

-- - az első $$[2, 4, 4, 6, 6, 6, 8, 8, 8, 8\ldots]$$,

szamokLs3 n i
  | i /= n = replicate i (i * 2) ++ szamokLs3 n (i + 1)
  | otherwise = replicate i (i * 2)

-- - az első $$[n, n-1, \ldots, 2, 1, 1, 2, \ldots, n-1, n]$$,

szamokLs4 n = [n, n - 1 .. 1] ++ [1 .. n]

szamokLs5 n = reverse [1 .. n] ++ [1 .. n]

-- - váltakozva tartalmazzon True és False értékeket,

tfLs n = take n ls
  where
    ls = [True, False] ++ ls

-- - váltakozva tartalmazza a $$0,\ 1,\ -1$$ értékeket.

szamokLs6 n = take n ls
  where
    ls = [0, 1, -1] ++ ls

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy adott szám osztóinak számát,

osztok n = [i | i <- [1 .. n], mod n i == 0]

osztokSzama n = length $ osztok n

osztokSzama2 n = myLength $ osztok n
  where
    myLength [] = 0
    myLength (_ : ls) = 1 + myLength ls

osztokSzama3 n = foldl (\res i -> if mod n i == 0 then res + 1 else res) 0 [1 .. n]

-- - meghatározza egy adott szám legnagyobb páratlan osztóját,

maxParatlanOszto n = last $ filter odd $ osztok n

maxParatlanOszto2 n = last [i | i <- [1, 3 .. n], mod n i == 0]

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,

decP x p
  | x < p = [x]
  | otherwise = decP (div x p) p ++ [mod x p]

decPszam x p = length $ decP x p

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,

decPmax x p = maximum $ decP x p

-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.

fibo = fiboSg 0 1 0
  where
    fiboSg a b res = res : fiboSg b res (res + b)

fiboAB a b = (dropWhile (< a) . takeWhile (< b)) fibo

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista pozitív elemeinek átlagát,

atlag ls = sum ls / fromIntegral (length ls)

pozAtlag ls = atlag [i | i <- ls, i > 0]

pozAtlag2 ls = (atlag . filter (> 0)) ls

-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,

listaN ls n = [i | (idx, i) <- zip [1 ..] ls, mod idx n == 0]

listaN2 ls n i
  | length ls < i = []
  | mod i n == 0 = ls !! i : listaN2 ls n (i + 1)
  | otherwise = listaN2 ls n (i + 1)

-- - tükrözi egy lista elemeit,

tukroz [] = []
tukroz (x : xs) = tukroz xs ++ [x]

-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,

maxPoziciokKetGordulessel xs =
  let m = maxElem xs
   in keres m xs 0
  where
    maxElem [x] = x
    maxElem (x : xs) = let m = maxElem xs in if x > m then x else m
    keres _ [] _ = []
    keres m (x : xs) i
      | x == m = i : keres m xs (i + 1)
      | otherwise = keres m xs (i + 1)

maxPoziciokEgyGordulessel (x : xs) = seged xs x 0 [0]
  where
    seged [] _ _ pozok = pozok
    seged (x : xs) maxVal i pozok
      | x > maxVal = seged xs x (i + 1) [i + 1]
      | x == maxVal = seged xs maxVal (i + 1) (pozok ++ [i + 1])
      | otherwise = seged xs maxVal (i + 1) pozok

-- - meghatározza egy lista leggyakrabban előforduló elemét.

leggyakoribb [] = error "ures lista"
leggyakoribb xs = legjobb (szamolMindent xs)
  where
    szamolMindent [] = []
    szamolMindent (y : ys) = (y, 1 + db y ys) : szamolMindent (szur y ys)
    db _ [] = 0
    db v (y : ys) = (if v == y then 1 else 0) + db v ys
    szur _ [] = []
    szur v (y : ys) = (if v == y then id else (y :)) (szur v ys)
    legjobb [(e, _)] = e
    legjobb ((e1, d1) : (e2, d2) : rest)
      | d1 >= d2 = legjobb ((e1, d1) : rest)
      | otherwise = legjobb ((e2, d2) : rest)
