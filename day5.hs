import Data.List (sort)

binarySearch :: Int -> Int -> String -> Char -> Int
binarySearch lo _ [] _ = lo
binarySearch lo hi (head:tail) lowerChar
    | head == lowerChar = binarySearch lo ((hi + lo) `div` 2) tail lowerChar
    | otherwise         = binarySearch ((hi + lo + 1) `div` 2) hi tail lowerChar

findRow :: String -> Int
findRow seq = binarySearch 0 127 seq 'F'

findCol :: String  -> Int
findCol seq = binarySearch 0 8 seq 'L'

skip :: Int -> String -> String
skip 0 s = s
skip n (_:tail) = skip (n-1) tail

findSeat :: String -> Int
findSeat s = (findRow (take 7 s)) * 8 + (findCol (skip 7 s))

findMissingSeat :: [Int] -> Int -> Int
findMissingSeat [] n = n
findMissingSeat (head:tail) n
    | head == n = findMissingSeat tail (n+1)
    | otherwise = n

main :: IO ()
main = do
    input <- getLine
    let inputs = sort $ map findSeat (read input :: [String])
    print $ foldr max 0 inputs
    print $ findMissingSeat inputs (head inputs)