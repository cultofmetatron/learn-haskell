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




