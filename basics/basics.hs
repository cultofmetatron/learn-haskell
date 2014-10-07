module Basics (
 factorial,
 myFilter
 ) where




doubleMe x = x + x

add5 x = x + 5

doubleUs x y = doubleMe x + doubleMe y

mult :: (Num a) => a -> a -> a -> a
mult x y z = x * y * z

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

divBy :: (Floating a) => a -> a -> a
divBy a = (/a)

myMap :: (a -> a) -> [a] -> [a]
myMap f [] = []
myMap f [a] = [(f a)]
myMap f (y:ys) = [(f y)] ++ (myMap f ys)

myReduce :: (a -> a -> a) -> a -> [a] -> a
myReduce f init [] = init
myReduce f init [y] = f init y
myReduce f init (y:ys) = myReduce f (f init y) ys

factorial :: (Enum a, Num a) => a -> a
factorial x = myReduce (*) 1 [1..x]

isEven :: (Ord a, Integral a) => a -> Bool
isEven x = (mod x 2) == 0

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f [y]
  | f y = [y]
  | otherwise = []
myFilter f (y:ys)
  | f y = y:(myFilter f ys)
  | otherwise = myFilter f ys
--myFilter f ys = myReduce (\check xs y -> if f y then xs ++ y else xs) [] ys


myEvery :: (a -> Bool) -> [a] -> Bool
myEvery f xs = (length xs) == (length (myFilter f xs))

takie :: (a -> Bool) -> [a] -> [a] -> [a]
takie p [] [] = []
takie p [] ys = ys
takie p (x:xs) ys
  | p x = takie p xs (ys ++ [x])
  | otherwise = ys

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile predicate xs = takie predicate xs []

summate :: (Num a) => [a] -> a
summate xs = foldl (\acc x -> acc + x) 0 xs


folder :: Int -> [Int] -> [Int] -> [Int]
folder _ [] xs = xs
folder n [x] xs
  | x < n = folder n [] (xs ++ [x])
  | otherwise = folder n [] xs
folder n (y:ys) xs
  | y < n = folder n ys (xs ++ [y])
  | otherwise = folder n ys xs

repeater:: Int -> Int  -> [Int]
repeater n i = map (\x -> i) [0..n]

--f :: Int -> [Int] -> [Int]
--f n arr = foldr (++) [] (map (repeater n) arr)

--factorial n = foldl (*) 1 [1..n]

computePoly :: Double -> Integer -> Double
computePoly _ 0 = fromIntegral 1
computePoly x 1 = x
computePoly x n = (x ^^ n) / (fromIntegral (factorial n))

solve :: Double -> Double
solve x = foldl (+) 0 (map (computePoly x) [0..10])
