main :: IO()
main = 
    print(intersectSet [2] [1, 2, 3])




appendList :: [Char] -> [Char] -> [Char]
appendList [] ys = ys
appendList xs [] = xs
appendList (x:xs) ys = 
    x:appendList xs ys


reverseList :: [Char] -> [Char]
reverseList [] = []
reverseList (x:xs) = 
    appendList (reverseList xs) [x]


takeList :: Int -> [Int] -> [Int]
takeList _ [] = []
takeList 0 xs = []
takeList n (x:xs) = 
    x : takeList (n-1) xs


dropList :: Int -> [Int] -> [Int]
dropList _ [] = []
dropList 0 xs = xs
dropList n (x:xs) =
    dropList (n-1) xs
    
    
splitList :: Int -> [Int] -> ([Int], [Int])
splitList n [] = ([], [])
splitList 0 xs = ([], xs)
splitList n (x:xs) = 
    (x:as, bs)
    where
    (as, bs) = splitList (n-1) xs


memberSet :: Int -> [Int] -> Bool
memberSet _ [] = False
memberSet n (x:xs)
    | n == x    = True
    | otherwise = memberSet n xs


unionSet :: [Int] -> [Int] -> [Int]
unionSet [] ys = ys
unionSet xs [] = xs
unionSet [] [] = []
unionSet (x:xs) ys
    | memberSet x ys = unionSet xs ys
    | otherwise      = x : unionSet xs ys
    
    
intersectSet :: [Int] -> [Int] -> [Int]
intersectSet [] _ = []
intersectSet _ [] = []
intersectSet [] [] = []
intersectSet (x:xs) ys
    | memberSet x ys = x : intersectSet xs ys
    | otherwise      = intersectSet xs ys










