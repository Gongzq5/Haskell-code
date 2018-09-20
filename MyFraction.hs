-- 巩泽群 16340063, 765194873@qq.com, 软件工程
{-.
其他说明：
    1. 测试了五个运算律，以及乘除法，加减法的正常转换
    2. 对于存在负数的结果展示进行了优化，分为以下几种情况(a,b为正数时)
        * (-a,-b)以及(a,b)展示为(a,b)
        * (a,-b)以及(-a,b)均展示为(-a,b)
    3. 加入代码使得quickCheck可以进行批量测试(见下边的说明)

        我使用了
        import Test.QuickCheck.All
        
        并且代码的最后有两行被注释掉的代码

        -- return []
        -- runTests = $quickCheckAll

        这个runTests可以一次运行所有prop_开头的测试函数

        若要启用此函数需要在cmd中加载时使用命令
        > gchi -XTemplateHaskell MyFraction.hs
        来加载文件

        进入gchi中即可使用
        > runTests
        来完成测试
-}

module MyFraction where
import Test.QuickCheck
import Test.QuickCheck.All

type Fraction = (Integer, Integer)

-------------------------- define of Fraction functions -----------------------

ratplus :: Fraction -> Fraction -> Fraction
ratplus (a, b) (c, d) = simplify((a*d+b*c), (b*d))

ratminus :: Fraction -> Fraction -> Fraction
ratminus (a, b) (c, d) = simplify((a*d-b*c), (b*d))

rattimes :: Fraction -> Fraction -> Fraction
rattimes (a, b) (c, d) = simplify((a*c), (b*d))

ratdiv :: Fraction -> Fraction -> Fraction
ratdiv (a, b) (c, d) = simplify((a*d), (b*c))

ratfloor :: Fraction -> Integer
ratfloor (a, b) = floor ((fromInteger a) / (fromInteger b))

ratfloat :: Fraction -> Float
ratfloat (a, b) = (fromInteger a) / (fromInteger b)

rateq :: Fraction -> Fraction -> Bool
rateq (a, b) (c, d) = simplify(a, b) == simplify (c, d)


-------------------------- define of help function ----------------------------

simplify :: Fraction -> Fraction
simplify (a, b) = 
    if (a*b) > 0 then
        (abs (a `div` e), abs (b `div` e))
    else 
        (-(abs (a `div` e)), abs (b `div` e))
    where
        e = abs (gcd a b)

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

prop_ratplus_unit :: Fraction -> Property
prop_ratplus_unit (a,b) = b > 0 ==>(a, b) <+> (0,1) <==> (a, b)

-- 乘法分配律
prop_rattimes_plus_distr :: Fraction -> Fraction -> Fraction -> Property
prop_rattimes_plus_distr (a,b) (c,d) (e,f) =
    b > 0 && d > 0 && f > 0 ==>
    (a,b) <-*-> ((c,d) <+> (e,f)) <==> ((a,b) <-*-> (c,d)) <+> ((a,b) <-*-> (e,f))

-- 加法结合律
prop_ratplus_associative :: Fraction -> Fraction -> Fraction -> Property
prop_ratplus_associative (a,b) (c,d) (e,f) =
    b > 0 && d > 0 && f > 0 ==>
    (a,b) <+> ((c,d) <+> (e,f)) <==> ((a,b) <+> (c,d)) <+> (e,f)

-- 乘法结合律
prop_rattimes_associative :: Fraction -> Fraction -> Fraction -> Property
prop_rattimes_associative (a,b) (c,d) (e,f) =
    b > 0 && d > 0 && f > 0 ==>
    (a,b) <-*-> ((c,d) <-*-> (e,f)) <==> ((a,b) <-*-> (c,d)) <-*-> (e,f)

-- 加法交换律
prop_ratplus_commutative :: Fraction -> Fraction -> Property
prop_ratplus_commutative (a,b) (c,d) = 
    b > 0 && d > 0 ==>
    (a, b) <+> (c, d) <==> (c, d) <+> (a, b) 

-- 乘法交换律
prop_rattimes_commutative :: Fraction -> Fraction -> Property
prop_rattimes_commutative (a,b) (c,d) = 
    b > 0 && d > 0 ==>
    (a, b) <-*-> (c, d) <==> (c, d) <-*-> (a, b) 

-- 除法和乘法的等价性
prop_times_div_equation :: Fraction -> Fraction -> Property
prop_times_div_equation (a,b) (c,d) =
    b /= 0 && d /= 0 && a /= 0 && c /= 0 ==>
    (a,b) <-*-> (c,d) <==> (a,b) </> (d,c)

-- 加法和减法的等价性
prop_plus_minus_equation :: Fraction -> Fraction -> Property
prop_plus_minus_equation (a,b) (c,d) =
    b /= 0 && d /= 0 && a /= 0 && c /= 0 ==>
    (a,b) <+> (c,d) <==> (a,b) <-> ((-c),d)

return []
runTests = $quickCheckAll