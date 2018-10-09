-- 巩泽群 16340063 765194873@qq.com 数据科学与计算机学院;

module Newton_Raphson where
import Test.QuickCheck

squareroot2 :: Float ->Integer ->Float
squareroot2 x0 0 = x0
squareroot2 x0 n = squareroot2 next (n-1)
    where
        next = ((x0 + 2 / x0) / 2)


squareroot :: Float -> Float -> Integer -> Float
squareroot r x0 0 = x0
squareroot r x0 n = squareroot r next (n-1)
    where
        next = ((x0 + r / x0) / 2)


sqrtSeq :: Float -> Float -> [Float]
sqrtSeq r x0 = x0:sqrtSeq r next
    where
        next = ((x0 + r / x0) / 2)


squareroot' :: Float -> Float -> Float -> Float
squareroot' r x0 epsilon = 
    if (abs (x0 - next) < epsilon) then
        next
    else
        squareroot' r next epsilon
    where
        next = ((x0 + r / x0) / 2)

----------------------------------- quick check -------------------------------

-- prop_sqrt2_sqrt :: Float -> Integer -> Property
-- prop_sqrt2_sqrt x0 n = n > 0 ==> squareroot2 x0 n == squareroot 2 x0 n


-- prop_sqrtSeq_squareroot :: Float -> Float -> Integer -> Property
-- prop_sqrtSeq_squareroot r x0 n = n > 0 && r > 0 ==> ((sqrtSeq r x0)!!(fromInteger n)) == squareroot r x0 n


-- main = do
--     quickCheck(prop_sqrt2_sqrt)
--     quickCheck(prop_sqrtSeq_squareroot)