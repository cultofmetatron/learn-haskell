
data TowerSet = TowerSet [Int] [Int] [Int] deriving (Show)
--swap Int -> Int -> (TowerSet [Int], [Int], [Int]) -> TowerSet([Int], [Int], [Int])
swap x tower
  | x == 'a' = x
  | x == 'b' = x
  | x == 'c' = x
