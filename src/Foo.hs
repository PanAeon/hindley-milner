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
