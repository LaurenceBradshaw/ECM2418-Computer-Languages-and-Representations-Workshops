main = do
    putStrLn "Incrementing 1"
    print (inc 1)
    putStrLn "Incrementing [1, 2, 3, 4]"
    print (incList [1..4])
    putStrLn "Negating True"
    print (neg True)
    putStrLn "Negating [True, True, False]"
    print (negList [True, True, False]) 
    putStrLn "Incrementing [15,20,25,30]"
    print (mapList inc [15,20,25,30])
    putStrLn "Is 8 a single?"
    print (single (-8))
    putStrLn "Finding singles in [6,9,15,21,3]"
    print (singleList [6,9,15,21,3])
    print (palindrome "racecar")
    print(palindromeList ["madam","spider","abba"])
    print(filterList single [8,3,12,18,5])
    print(partitionList (<= 6) [8,2,9,5,4,1,6])
    print(partitionList2 (<= 6) [8,2,9,5,4,1,6])
    

inc :: Num a => a -> a
inc = (+) 1

incList :: Num a => [a] -> [a]
incList [] = []
incList (x:xs) = inc x : incList xs

neg :: Bool -> Bool
neg a = not a

negList :: [Bool] -> [Bool]
negList [] = []
negList (x:xs) = neg x : negList xs

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

single :: Int -> Bool
single x
    | abs x < 10 = True
    | otherwise = False

singleList :: [Int] -> [Int]
singleList [] = []
singleList (x:xs) 
    | single x  = x : singleList xs
    | otherwise = singleList xs

palindrome :: [Char] -> Bool
palindrome [] = True
palindrome [x] = True
palindrome (x:xs)
    | x == last xs = palindrome (init xs)
    | otherwise = False
    
palindromeList :: [[Char]] -> [[Char]]
palindromeList [] = []
palindromeList (x:xs) 
    | palindrome x = x : palindromeList xs
    | otherwise = palindromeList xs
    
filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList p (x:xs)
    | p x       = x : filterList p xs
    | otherwise = filterList p xs

partitionList :: (a -> Bool) -> [a] -> ([a], [a])
partitionList _ [] = ([], [])
partitionList p x = (filterList p x, filterList (not.p) x)

partitionList2 :: (a -> Bool) -> [a] -> ([a],[a])
partitionList2 _ [] = ([], [])
partitionList2 p list = ([x | x <- list, p x], [x | x <- list, (not.p) x])






