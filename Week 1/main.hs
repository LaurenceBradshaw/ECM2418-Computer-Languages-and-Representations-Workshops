
main = do
    putStrLn ("Ledngth list of [1, 2, 3, 4, 5]: ")
    print (lengthList [1, 2, 3, 4, 5])
    putStrLn ("Length list of []: ")
    print (lengthList [])
    putStrLn ("List not containing element:")
    print (containsList ["TO","BE","OR","NOT","TO","BE"] "THAT")
    putStrLn ("List containing element:")
    print (containsList ["TO","BE","OR","NOT","TO","BE"] "NOT")
    putStrLn ("List which is a set:")
    print (isSetList  ["A","B","C","D","E","F"])
    putStrLn ("List which is not a set")
    print (isSetList  ["TO","BE","OR","NOT","TO","BE"])
    putStrLn ("Largest element in [3,7,5,9,2]:")
    print (largestList [3,7,5,9,2])
    putStrLn ("Largest element in [3]:")
    print (largestList [3])
    putStrLn ("Insert 4 into [2,3,5,7,11]:")
    print (insert 4 [2,3,5,7,11])
    putStrLn ("Insert 8 into [2,3,5,7,11]:")
    print (insert 8 [2,3,5,7,11])
    putStrLn ("Insert 4 into [2]:")
    print (insert 4 [2])
    putStrLn ("Insert 4 into []:")
    print (insert 4 [])
    putStrLn ("Sorting [4,2,11,7,3,5]:")
    print (sortList [4,2,11,7,3,5])
    putStrLn ("Sorting [4,2]:")
    print (sortList [4,2])
    putStrLn ("Sorting [4]:")
    print (sortList [4])
    putStrLn ("Sorting []:")
    print (sortList [])
    putStrLn ("Zipping [\"France\",\"England\",\"Japan\"], [33,44,81]:")
    print (zipped (["France","England","Japan"], [33,44,81]))
    
    
    
-- FUNCTIONS --

-- length x
lengthList :: [Int] -> Int
lengthList [] = 0
lengthList (x:xs) = 
    1 + lengthList(xs)
    
    
-- 1 `elem` [1,4,5,7]
containsList :: [String] -> String -> Bool
containsList [] _ = False
-- containsList (x:xs) s = 
--     if x == s then True
--     else containsList xs s
containsList (x:xs) s 
    | x == s    = True
    | otherwise = containsList xs s


isSetList :: [String] -> Bool
isSetList [] = True
-- isSetList (x:xs) = 
--     if containsList xs x then False
--     else isSetList xs
isSetList (x:xs) 
    | containsList xs x = False
    | otherwise         = isSetList xs
    
    
-- maximum [3,2,6,4,1,2,3]
largestList :: [Int] -> Int
largestList [] = error "No elements in list"
largestList [x] = x
-- largestList (x:xs) =
--     if x > head xs then largestList (x:tail xs)
--     else largestList xs
largestList (x:xs)
    | x > head xs = largestList (x:tail xs)
    | otherwise   = largestList xs


insert :: Int -> [Int] -> [Int]
insert e [] = [e]
insert e [x] = if e < x then e:[x] else [x] ++ [e]
-- insert e (x:xs) =
--     if e < x then e:x:xs
--     else x:(insert e xs)
insert e (x:xs)
    | e < x     = e:x:xs
    | otherwise = x:(insert e xs)


-- import Data.List
-- sort list_name
sortList :: [Int] -> [Int]
sortList [] = []
sortList [x] = [x]
sortList (x:xs) =
    insert x (sortList xs)


-- zip [1,2,3] [9,8,7]
zipped :: ([a], [b]) -> [(a, b)]
zipped ([], []) = []
zipped ([x], [y]) = [(x, y)]
zipped ((x:xs), (y:ys))
    = [(x, y)] ++ zipped (xs, ys)




