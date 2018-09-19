-- 巩泽群 16340063, 765194873@qq.com, 软件工程

module MyFraction where
import Test.QuickCheck

type Fraction = (Integer, Integer)

ratplus :: Fraction -> Fraction -> Fraction
ratplus (a, b) (c, d) = ((a*d+b*c) `div` e, (b*d) `div` e)
    where 
    e = gcd (abs(a*d+b*c)) (abs(b*d))

ratminus :: Fraction -> Fraction -> Fraction
ratminus (a, b) (c, d) = ((a*d-b*c) `div` e, (b*d) `div` e)
    where 
    e = gcd (a*d-b*c) (b*d)

rattimes :: Fraction -> Fraction -> Fraction
rattimes (a, b) (c, d) = ((a*c) `div` e, (b*d) `div` e)
    where
    e = gcd (a*c) (b*d)

ratdiv :: Fraction -> Fraction -> Fraction
ratdiv (a, b) (c, d) = ((a*d) `div` e, (b*c) `div` e)
    where
    e = gcd (a*d) (b*c)

ratfloor :: Fraction -> Integer
ratfloor (a, b) = floor ((fromInteger a) / (fromInteger b))

ratfloat :: Fraction -> Float
ratfloat (a, b) = (fromInteger a) / (fromInteger b)

rateq :: Fraction -> Fraction -> Bool
rateq (a, b) (c, d) = (((a `div` e1) == (c `div` e2)) && ((b `div` e1) == (d `div` e2)) 
    || (a == 0 && c == 0))
    where 
    e1 = gcd a b
    e2 = gcd c d

------------------------------ define of operators ----------------------------

infix 5 <+>
(<+>) :: Fraction -> Fraction -> Fraction
(<+>) (a,b) (c,d) = ratplus (a,b) (c,d)

infix 6 <-*->
(<-*->) :: Fraction -> Fraction -> Fraction
(<-*->) (a,b) (c,d) = rattimes (a,b) (c,d)

infix 5 <->
(<->) :: Fraction -> Fraction -> Fraction
(<->) (a,b) (c,d) = ratminus (a,b) (c,d)

infix 6 </>
(</>) :: Fraction -> Fraction -> Fraction
(</>) (a,b) (c,d) = ratdiv (a,b) (c,d)

infix 4 <==>
(<==>) :: Fraction -> Fraction -> Bool
(<==>) (a,b) (c,d) = rateq (a,b) (c,d) 

----------------------------- functions for quickcheck ------------------------

prop_ratplus (a, b) = b /= 0 ==> (a, b) <+> (a, b) <==> (2*a, b)

-- prop_ratminus :: Fraction -> property
-- prop_ratminus (a, b) = b <> 0 ==> (a, b) <-> (-1, b) <==> (a+1, b)

-- prop_rattimes :: Fraction -> property
-- prop_rattimes (a, b) = b <> 0 ==> (a, b) <-*-> (0, 1) <==> (0, 1)

-- prop_ratdiv :: Fraction -> property
-- prop_ratdiv (a, b) (c, d) = (a*d `div` e, b*c `div` e)
--     where
--     e = gcd (a*d) (b*c)

-- prop_ratfloor :: Fraction -> property
-- prop_ratfloor (a, b) = floor (fromInteger a / fromInteger b)

-- prop_ratfloat :: Fraction -> property
-- prop_ratfloat (a, b) = fromInteger a / fromInteger b

-- prop_rateq :: Fraction -> property
-- prop_rateq (a, b) (c, d) = a `div` e1 == b `div` e1 && c `div` e2 == d `div` e2
--     where 
--     e1 = gcd a b
--     e2 = gcd c d
