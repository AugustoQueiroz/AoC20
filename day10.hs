import Data.List (sort)

day10 :: [Int] -> Int -> (Int, Int) -> (Int, Int)
day10 [] _ result = result
day10 (head:tail) lastValue (c1, c3)
    | head - lastValue == 1 = day10 tail head (c1+1, c3)
    | head - lastValue == 3 = day10 tail head (c1, c3+1)
    | otherwise = day10 tail head (c1, c3)

main :: IO ()
main = do
    input <- getLine
    let inputs = sort (read input :: [Int])
    let (c1, c3) = day10 inputs 0 (0, 0)
    print $ c1 * (c3+1)