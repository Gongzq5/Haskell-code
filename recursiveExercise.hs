-- Excersice

-- Recursive excersice
-- http://elearning.sysu.edu.cn/webapps/blackboard/execute/displayLearningUnit?course_id=_11359_1&content_id=_257011_1
-- 2018-09-24
-- by gongzq

import Test.QuickCheck

myGCD :: Integer -> Integer -> Integer
myGCD x y = if 0 == y then x
            else if 0 == x then y
            else if x < y then myGCD x (y-x) 
            else myGCD (x-y) y

fac :: Integer -> Integer
fac x = if x <= 1 then 1
        else fac (x-1) * x

sumFacs :: Integer -> Integer
sumFacs n = if n <= 0 then fac 0
            else sumFacs (n-1) + fac n
            
sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n = if n <= 0 then f 0
             else sumFun f (n-1) + f n
             
prop_myGCD :: Integer -> Integer -> Property
prop_myGCD x y = x > 0 && y > 0 ==> myGCD x y == gcd x y