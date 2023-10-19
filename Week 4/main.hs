import Data.List

main :: IO()
main = print(filter tester generator)


generator :: [(Int, Int, Int)]
generator = [(a, b, c) | a <- [145..179], b <- [245..269], c <- [345..359]]
-- The range of numbers to generate can be shortend due to the fact that each number must appear once

tester :: (Int, Int, Int) -> Bool
tester (a, b, c) = covered (a, b, c) && not(prime a) && not(prime b) && not(prime c)
    where
    prime :: Int -> Bool
    prime = not . factorisable 2
        where
        factorisable :: Int -> Int -> Bool
        factorisable f n
            | f * f <= n = n `mod` f == 0 || factorisable (f + 1) n
            | otherwise  = False
    
    covered :: (Int, Int, Int) -> Bool
    covered (a, b, c) = ("123456789" \\ (show a ++ show b ++ show c)) == []
                












