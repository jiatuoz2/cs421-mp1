--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake n (x:xs) 
    | n <= 0 = []
    | otherwise = x : mytake (n-1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n (x:xs) 
    | n <= 0 = x:xs
    | otherwise = mydrop (n-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a] 
rev [] = []
rev xs = last xs : rev (init xs)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] xs = xs
app xs [] = xs
app (a:as) xs = a : app as xs

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = x + 1 : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (a:as) (b:bs) = (a, b) : myzip as bs

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xs ys = 
    let zip = myzip xs ys
        f [] = []
        f (x:xs) = (fst x + snd x) : f xs
    in f zip

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 
    let f :: [Integer] -> [Integer]
        f pair = 
            let first = pair !! 0
                second = pair !! 1
                third = addpairs [first] [second]
                npair = tail (first : second : third)
            in first : f npair 
    in f [0, 1]


--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add a [] = [a]
add a (x:xs) 
    | a == x = x : xs
    | a < x = a : x : xs 
    | a > x = x : add a xs

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a] 
union [] xs = xs
union xs [] = xs
union s1 s2 = 
    let merge :: Ord a => [a] -> [a] -> [a]
        merge [] xs = xs
        merge xs [] = xs
        merge (x:xs) (y:ys)
            | x <= y = x : merge xs (y:ys)
            | x > y = y : merge (x:xs) ys
        eliminate :: Ord a => [a] -> [a]
        eliminate [] = []
        eliminate [x] = [x]
        eliminate (x1:x2:xs) 
            | x1 == x2 = eliminate (x2:xs)
            | otherwise = x1 : eliminate (x2:xs)
    in eliminate (merge s1 s2)

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect s1 s2 = 
    let merge :: Ord a => [a] -> [a] -> [a]
        merge [] xs = xs
        merge xs [] = xs
        merge (x:xs) (y:ys)
            | x <= y = x : merge xs (y:ys)
            | x > y = y : merge (x:xs) ys
        duplicate :: Ord a => [a] -> [a]
        duplicate [] = []
        duplicate [x] = []
        duplicate [x1, x2] 
            | x1 == x2 = [x2]
            | otherwise = []
        duplicate (x1:x2:x3:xs)
            | x1 == x2 && x2 /= x3 = x2 : duplicate (x3:xs)
            | otherwise = duplicate (x2:x3:xs)
    in duplicate (merge s1 s2)

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset s = 
    let f :: Ord a => [[a]] -> [a] -> [[a]]
        f acc [] = acc
        f acc (x:xs) = 
            let map' :: Ord a => ([a] -> [a]) -> [[a]] -> [[a]]
                map' _ [] = []
                map' func (a:as) = func a : map' func as
                newacc = map' (add x) acc
            in f (union acc newacc) xs
    in f [[]] (rev s)

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' xs = P.foldr (\x acc -> (x + 1) : acc) [] xs

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: Num a => [a] -> a
sumlist' xs = P.foldl (\acc x -> acc + x) 0 xs
