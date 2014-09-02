--helpers
factorial :: (Enum a, Num a) => a -> a
factorial n =  foldl (*) 1 [0..n]

--basic trig functions for workign with complex numbers
data Complex a = Complex a a deriving (Show)

complexPlus :: (Num t) => Complex t -> Complex t -> Complex t
(Complex r1 i1) `complexPlus` (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

--taylorPolySine :: (Fractional b, Integral b) => b -> b -> b
taylorPolySine theta n =  (((-1) ^^ n) / (factorial ((2 * n) + 1))) * (theta ^^ (2 * n + 1))



--taylor_sine = [x | x <- [0..]  ]

