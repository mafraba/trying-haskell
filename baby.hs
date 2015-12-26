doubleMe x = x + x

doubleUs x y = x*2 + y*2

-- Splits a list into two halves
halve :: [a] -> ([a], [a])
halve x = (take n x, drop n x)
          where n = length x `div` 2


-- Finds all pythagorean triplets under certain limit
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z)| x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z ]


--
scalarproduct :: [ Int ] -> [ Int ] -> Int
scalarproduct x y = sum [ u * v | (u,v) <- zip x y ]


-- Merge two sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) = if x < y then (x:xtaken) else (y:ytaken)
                      where
                        xtaken = merge xs (y:ys)
                        ytaken = merge (x:xs) ys

-- Mergesort
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge s1 s2
           where
             (h1, h2) = halve xs
             s1 = msort h1
             s2 = msort h2
