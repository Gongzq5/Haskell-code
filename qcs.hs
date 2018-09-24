-- Excersice

-- quick check excersice
-- 2018-9-18
-- by gongzq

import Test.QuickCheck

maxThree :: Float -> Float -> Float -> Float
maxThree x y z = 
    if x > y then
        if x > z then 
            x
        else
            z
    else 
        if y > z then 
            y
        else
            z
            
-- maxThree x y z = max x (max y z)

prop_maxThree x y z = maxThree x y z >= x && maxThree x y z >= y && maxThree x y z >= z
