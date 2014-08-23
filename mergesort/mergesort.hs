--mergesort in haskell


--[a] =>
merge :: (Ord a) => [a] -> [a] -> [a] -> [a]
merge []  []  acc = acc
merge [x] []  acc = acc ++ [x]
merge []  [y] acc = acc ++ [y]
merge xs  []  acc = acc ++ xs
merge []  ys  acc = acc ++ ys
merge xs ys acc
  | (head xs) <= (head ys) = merge (tail xs) ys (acc ++ [(head xs)])
  | (head xs) > (head ys) = merge xs (tail ys) (acc ++ [(head ys)])


middleIndex :: (Ord a) => [a] -> Int
middleIndex as = floor ((length as) / 2)




--slice a -> a -> [b] -> [b]



--mergeSort :: (Ord a) => [a] -> [a]



--[(x, y) | x <- ["rock", "paper", "scizzors"], y <- ["rock", "paper", "scizzors"]]

--mergesort [] = []
--mergesort [a, b]
--  | a > b = [b, a]
--  | a < b = [a, b]

