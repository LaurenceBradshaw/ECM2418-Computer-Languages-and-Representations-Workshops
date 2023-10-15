import Data.List

main :: IO()
main = do
    print(filter tester generator)


generator :: [(Int, Int, Int)]
generator = [ (x, y, z) | x <- a, y <- b, z <- c]
    where
        a = [91..180]
        b = [181..270]
        c = [271..360]


tester :: (Int, Int, Int) -> Bool
tester (x, y, z) = noPrimes && isCovered
    where
        noPrimes = (not . any prime) [x, y, z]
        isCovered = covered (x, y, z)

        prime :: Int -> Bool
        prime 0 = False
        prime 1 = False
        prime x = all not [ x `mod` n == 0 | n <- [2..(isqrt x)]]
            where
                isqrt :: Int -> Int
                isqrt = ceiling . sqrt. fromIntegral

        covered :: (Int, Int, Int) -> Bool
        covered (x, y, z) = and [ a `elem` nums | a <- oneToNine]
            where
                nums = show x ++ show y ++ show z
                oneToNine = concat [show e | e <- [1..9]]




xGenerator :: Int
xGenerator = length [ t | t <- ts , t `elem` g ]
    where
        g = generator
        ts = [ (145 ,245 ,345)
            , (147 ,261 ,355)
            , (150 ,253 ,350)
            , (151 ,261 ,355)
            , (153 ,245 ,345)
            , (154 ,253 ,350)
            , (155 ,261 ,355)
            , (157 ,245 ,345)
            , (158 ,253 ,350)
            , (162 ,253 ,350)]
        

xTester :: Int
xTester = length [ t | t <- ts , tester t ]
    where
        ts = [ (159 ,267 ,348)
            , (168 ,249 ,357)
            , (176 ,249 ,358)
            , (176 ,259 ,348)
            , (178 ,249 ,356)
            , (178 ,259 ,346) ]
                












