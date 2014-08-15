

mygcd a b = if (mod a b) == 0 then b
          else if (mod b a) == 0 then a
          else if a > b then gcd b (mod a b)
          else gcd a (mod b a)
