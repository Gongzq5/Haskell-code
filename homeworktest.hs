f0 :: Int -> Bool
f0 x = _ || _

f1 :: Int -> Int 
f1 x = x + 1

f2 :: (Int -> Int) -> Int
f2 x = x 2

pair :: Num a => Num b => a -> b -> (a, b)
pair a b = (a, b)
