append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]

nth :: [a] -> Int -> a
nth (x:xs) n | n == 0 = x
             | n > 0  = nth xs (n - 1)
