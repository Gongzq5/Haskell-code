-- Edit by gongzq
-- Last modified in 2018-10-12

module Pictures where

    type Picture = [[Char]] 

    -- above, 
    sideBySide    :: Picture -> Picture -> Picture
    flipH, flipV :: Picture -> Picture
    -- invertColour         :: Picture -> Picture
    -- superimpose          :: Picture -> Picture -> Picture
    printPicture         :: Picture -> IO ()

    flipH pic = [reverse x | x <- pic]

    flipV = reverse

    sideBySide pic1 pic2 = [ line1 ++ line2 | (line1,line2) <- zip pic1 pic2 ]
    -- pic1 `sideBySide` pic2 = [ line1 ++ line2 | (line1,line2) <- zip pic1 pic2 ]

    printPicture pic = putStr ( unlines pic )

    apic = ["#######",
            "#######",
            "     ##",
            "    ## ",
            "   ##  ",
            "  ##   ",
            "  ##   "];