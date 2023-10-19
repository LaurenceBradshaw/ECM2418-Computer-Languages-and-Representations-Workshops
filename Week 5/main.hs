import Data.List

main :: IO()
main = print (filter tester generator)

-- First hymn is a perfect square
-- at least one other perfect square
-- Sum of the 4 hymns is prime
-- Given the first digit of the prime number what is the first hymn number? ("961","691","619","916") – 3187

generator :: [(String, String, String, String)]
generator = map listToTuple (concatMap (subsets 4 . permutations) s1)  -- concatMap f = concat . map f
    where
        subsets :: Int -> [String] -> [[String]]
        subsets 0 _ = [[]]
        subsets _ [] = []
        subsets n (x:xs) = map (x :) (subsets (n-1) xs) ++ subsets n xs

        special :: String -> Bool
        special s = noZero && unique s
            where
                noZero = '0' `notElem` s

                unique :: String -> Bool
                unique [] = True
                unique (x:xs) = x `notElem` xs && unique xs

        s1 :: [String]
        s1 = [show x | x <- [123..987], special (show x)]

        listToTuple :: [String] -> (String, String, String, String)
        listToTuple [a, b, c, d] = (a, b, c, d)

-- "123" to "987"
-- Generate a 'speical' s1
-- Generate all permutations of s1
-- take every subset of size 4 from permutations of s1

tester :: (String, String, String, String) -> Bool
tester (a, b, c, d) = perfectSquare a && anotherPerfectSquare (a, b, c, d) && prime (sum [read a, read b, read c, read d])
    where
        prime :: Int -> Bool
        prime = not . factorisable 2
            where
            factorisable :: Int -> Int -> Bool
            factorisable f n
                | f * f <= n = n `mod` f == 0 || factorisable (f + 1) n
                | otherwise  = False

        anotherPerfectSquare :: (String, String, String, String) -> Bool
        anotherPerfectSquare (a, b, c, d) = any perfectSquare (permutations a \\ [a, b, c, d])

        perfectSquare :: String -> Bool
        perfectSquare x = rootX == fromInteger (round rootX)
            where
                rootX = sqrt (fromIntegral (read x))


xGenerator :: Int
xGenerator = length [ t | t <- ts , t `elem` g ]
    where
        g = generator
        ts =[("123","213","321","231")
            , ("168","618","861","186")
            , ("236","326","263","623")
            , ("284","824","482","842")
            , ("351","531","153","315")
            , ("397","937","379","739")
            , ("467","647","764","674")
            , ("524","254","425","542")
            , ("581","851","518","158")
            , ("639","369","936","396")]

xTester :: Int
xTester = length [ t | t <- ts , tester t ]
    where
        ts =[("196","691","961","619")
            , ("196","619","691","961")
            , ("256","526","652","265")
            , ("256","526","562","265")
            , ("256","652","265","526")
            , ("961","691","619","196")
            , ("961","691","196","619")
            , ("961","196","619","691")
            , ("961","196","691","619")
            , ("961","916","619","691")]

