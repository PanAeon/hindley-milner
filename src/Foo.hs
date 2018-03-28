{-# LANGUAGE LambdaCase #-}
module Foo(quicksort) where

import Data.List
import Data.Vector(Vector)
import qualified Data.Vector as V

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort as) ++ x:(quicksort bs)
           where
             (as,bs) = partition (< x) xs

quickselect :: Ord a => Int -> [a] -> a
-- quickselect _ (x:[]) = x
quickselect n (x:xs) = if pivotindex == n
                       then x
                       else if n < pivotindex
                            then quickselect n ls
                            else quickselect (n - pivotindex - 1) (rs')
  where
    (ls, rs') = partition (< x) xs
    pivotindex = length ls

findMinimum :: [Int] -> Int
findMinimum = findMinimum' 0

-- assumption - unique elements
findMinimum' :: Int -> [Int] -> Int
findMinimum' loffset [] = loffset + 1
findMinimum' loffset (x:xs) = if loffset + length ls  + 1 == x
then findMinimum' x rs
else findMinimum' loffset ls
  where
    (ls, rs) = partition (< x) xs


-- yeah!!
-- now the implementation from the "Pearl of Functional Algorithm design"

-- quadratic
minfree :: [Int] -> Int
minfree xs = head ([1..] \\ xs)

-- array based

search :: Vector Bool -> Int
search = length . (V.takeWhile id)

-- yeah!
minfree' :: [Int] -> Int
minfree' xs = search v'
  where
    n = length xs
    v =  V.replicate n False
    xs' = (\x -> (x,True))<$> filter (<= n) xs
    v' = V.update v (V.fromList xs')

-----------------------------------------------
-- maximum surpaser
-- looks odd, but probably correct, at least to O(n log n)
maxSurpasser:: Ord a => [a] -> Int
maxSurpasser xs = maximum (ys)
  where
    xs' = (\case {(j, (i, v)) -> (j,i,v)}) <$> (zip [0..] $ sortOn snd (zip [0..] xs))
    l   = length xs'
    xs'' = groupBy g xs'
    g (_, _, v0) (_, _, v1) = v0 == v1
    ys = h <$> xs''
    h xs = let
            (j, i, _) = head xs
           in  l - j -i - (length xs)

msc :: Ord a => [a] -> Int
msc xs = maximum [scount z zs | z:zs <- tails' xs]

scount z zs = length $ filter (>z) zs

tails' [] = []
tails' (x:xs) = (x:xs) : tails xs

---------------------------------
table :: Ord a => [a] -> [(a,Int)]
table [x] = [(x, 0)]
table xs = join' (m - n) (table ys) (table zs)
  where
    m = length xs
    n = m `div` 2
    (ys, zs) = splitAt n xs
  --sortOn fst [(z, scount z zs) | z:zs <- tails' xs]
-- tails (xs ++ ys) = map (++ys) (tails xs) ++ tails ys
{-
table (xs ++ ys)
= {definition}   [(z, scount z zs) | z:zs <- tails' (xs ++ ys)]
= {d&c prop}    [(z, scount z zs) | z:zs <- map (++ys) (tails xs) ++ tails ys]
= {distrib}    [(z, scount z zs + scount z ys) | z:zs <- tails xs] ++ [(z, scount z zs) | z:zs <- tails' ys]
= {...}        [(z, c + scount z (map fst table ys)) | (z,c) <- table xs] ++ table ys
-}

-- FIXME: revisit this 
join' 0 txs [] = txs
join' n txs [] = error ("Foo! " ++ show n)
join' n [] tys = tys
join' n txs@((x,c):txs') tys@((y,d):tys')
  | x < y = (x,c + n) : join' n txs' tys
  | x >= y = (y,d): join' (n - 1) txs tys'

-- join' txs tys = [(x, c + tcount x tys) | (x,c) <- txs] vv tys
-- tcount z tys = length (dropWhile((z>=) . fst) tys)
