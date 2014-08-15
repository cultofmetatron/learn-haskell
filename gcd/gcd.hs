
-- iterative version
iter_gcd a b
  | a == b = a
  | (a > b) && (mod a b) == 0 = b
  | (a < b) && (mod b a) == 0 = a
  | (a > b) = iter_gcd b (mod a b)
  | (a < b) = iter_gcd a (mod b a)

mygcd a b = if (mod a b) == 0 then b
  else if (mod b a) == 0 then a
  else if a > b then gcd b (mod a b)
  else gcd a (mod b a)


