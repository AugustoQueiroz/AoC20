{-
    Movement
-}
moveInDirection :: (Int, Int, Int) -> (Char, Int) -> (Int, Int, Int)
moveInDirection (x, y, r) (direction, delta)
    | direction == 'N' = (x, y+delta, r)
    | direction == 'S' = (x, y-delta, r)
    | direction == 'E' = (x+delta, y, r)
    | direction == 'W' = (x-delta, y, r)

moveForwards :: (Int, Int, Int) -> Int -> (Int, Int, Int)
moveForwards (x, y, r) delta
    | r == 0 = moveInDirection (x, y, r) ('N', delta)
    | r == 90 = moveInDirection (x, y, r) ('E', delta)
    | r == 180 = moveInDirection (x, y, r) ('S', delta)
    | r == 270 = moveInDirection (x, y, r) ('W', delta)

angleRotation :: Int -> Int -> Int
angleRotation r delta
    | r + delta < 0 = r + delta + 360
    | r + delta >= 360 = r + delta - 360
    | otherwise = r + delta

rotateShip :: (Int, Int, Int) -> (Char, Int) -> (Int, Int, Int)
rotateShip (x, y, r) (direction, delta)
    | direction == 'R' = (x, y, angleRotation r delta)
    | direction == 'L' = (x, y, angleRotation r (-delta))

performMovement :: (Int, Int, Int) -> (Char, Int) -> (Int, Int, Int)
performMovement shipState (move, delta)
    | move == 'F'                = moveForwards shipState delta
    | move == 'R' || move == 'L' = rotateShip shipState (move, delta)
    | otherwise                  = moveInDirection shipState (move, delta)

performMovements :: (Int, Int, Int) -> [(Char, Int)] -> (Int, Int, Int)
performMovements = foldl performMovement

{-
    Input
-}
parseMovement :: String -> (Char, Int)
parseMovement s = (head s, read (tail s) :: Int)

{-
    Main
-}
main :: IO ()
main = do
    input <- getLine
    let movements = map parseMovement (read input :: [String])
    let (x, y, _) = performMovements (0, 0, 90) movements
    print $ abs x + abs y