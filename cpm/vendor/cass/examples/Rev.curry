append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]

fromTo :: Int -> Int -> [Int]
fromTo m n | m <= n    = m : fromTo (m+1) n
           | otherwise = []

main :: Int -> Int -> [Int]
main m n = rev (fromTo m n)
