
f1 :: Int -> Bool 
f1 x = b || c
   | b==c  = c
   | otherwise = True
 

f2 :: (Int -> Int) -> Int
f2 x = x 2

pair :: Num a => Num b => a -> b -> (a, b)
pair a b = (a, b)

halve xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
    where n = length xs 