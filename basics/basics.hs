doubleMe x = x + x

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



