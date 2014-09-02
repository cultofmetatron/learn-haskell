--helpers
factorial :: Integer -> Integer
factorial n =  foldl (*) 1 [1..n]

--basic trig functions for workign with complex numbers
data Complex a = Complex a a deriving (Show)

complexPlus :: (Num t) => Complex t -> Complex t -> Complex t
(Complex r1 i1) `complexPlus` (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

taylorPolySine :: Double -> Integer -> Double
taylorPolySine theta n =  (((-1) ^^ n) / (fromIntegral (factorial ((2 * n) + 1)))) * (theta ^^ (2 * n + 1))

sine theta = sum $ map (taylorPolySine theta) [n | n <- [0..100]]

taylorPolyCosine :: Double -> Integer -> Double
taylorPolyCosine theta n = (((-1) ^^ n)/(fromIntegral $ factorial (2 * n))) * (theta ^^ (2 * n))

cosine theta = sum $ map (taylorPolyCosine theta) [n | n <- [0..100]]

--taylorPolyTangent :: Double -> Integer -> Double
--taylorPolyTangent theta n = 


