-- Excersice

-- Recursive excersice
-- http://elearning.sysu.edu.cn/webapps/blackboard/execute/displayLearningUnit?course_id=_11359_1&content_id=_257011_1
-- 2018-10-01
-- by gongzq

import Test.QuickCheck

mygcd :: Integer -> Integer -> Integer
mygcd x y = if 0 == y then x
            else if 0 == x then y
            else if x < y then mygcd x (y-x) 
            else mygcd (x-y) y

fac :: Integer -> Integer
fac x = if x <= 1 then 1
        else fac (x-1) * x

sumFacs :: Integer -> Integer
sumFacs n = if n <= 0 then fac 0
            else sumFacs (n-1) + fac n
            
sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n = if n <= 0 then f 0
             else sumFun f (n-1) + f n

maxFun :: (Integer -> Integer) -> Integer -> Integer
maxFun f n = if n == 1 then max (f 1) (f 0)
             else max (f n) (maxFun f (n-1))

fib :: Integer ->Integer
fib n = if 0 == n then 0
        else if 1 == n then 1
        else fib (n-1) + fib (n-2)

sqrt2 :: Float ->Integer ->Float
sqrt2 x0 n = if 0 == n then x0
             else sqrt2 ((x0 + 2 / x0) / 2) (n-1)

solve :: Float -> Float -> Float -> (Float, Float)
solve a b c = if delta < 0 then error "discriminant smaller than 0, so the function has no answer"
              else ( ( (-b + sqrt delta) / (2*a) ) ,  (-b - sqrt delta) / (2*a)  )
              where
                delta = (b*b - 4*a*c)