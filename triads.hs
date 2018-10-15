triands :: Int -> [(Int, Int, Int)]
triands n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

triands2 :: Int -> [(Int, Int, Int)]
triands2 n = [(x, y, z) | x <- [1..n], y <- [x..n], z <- [1..n], x^2 + y^2 == z^2]
