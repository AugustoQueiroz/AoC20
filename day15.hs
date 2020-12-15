findNextNumber :: [Int] -> Int
findNextNumber l = lastAppearanceOf (reverse (init l)) (last l) 1

lastAppearanceOf :: [Int] -> Int -> Int -> Int
lastAppearanceOf [] _ _ = 0 -- Does not appear
lastAppearanceOf (h:t) i d
    | h == i = d
    | otherwise = lastAppearanceOf t i (d+1)

find2020th :: [Int] -> Int -> Int
find2020th l i
    | i+1 == 2020 = findNextNumber l
    | otherwise   = find2020th (l ++ [findNextNumber l]) (i+1)
