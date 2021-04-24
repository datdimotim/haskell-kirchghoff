module Kramer where


kramer :: [[Int]] -> [Int] -> [Rational]
kramer m b = let
               d = det m
               n = length m
             in
               map ((/ (fromInteger . toInteger) d) . fromInteger . toInteger . det) 
               . map (\i -> replaceCol i b m)
                $ [0 .. n - 1]

replaceCol :: Int -> [Int] -> [[Int]] -> [[Int]]
replaceCol i cs = map (\(c, ln) -> replaceAt i c ln) . zip cs

det :: [[Int]] -> Int
det [[a]] = a
det (m@(l:ls)) = sum . map (\(i, a) -> (minorSign i 0) * a * det (minor i 0 m)) $ zip [0..] l

minorSign :: Int -> Int -> Int
minorSign c r = 1 - ((c + r) `mod` 2) * 2

minor :: Int -> Int -> [[a]] -> [[a]]
minor c r = map (dropAt c) . dropAt r

dropAt :: Int -> [a] -> [a]
dropAt i = (\(s,f) -> s ++ tail f) . splitAt i

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i a = (\(s,f) -> s ++ [a] ++ tail f) . splitAt i
