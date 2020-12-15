import Data.List (sort)

listHas :: [Int] -> Int -> Bool
listHas [] _ = False
listHas (head : tail) num
  | head == num = True
  | otherwise = listHas tail num

-- Part 1
twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum [] _ = Nothing
twoSum (head : tail) sum
  | listHas tail (sum - head) = Just (head, sum - head)
  | otherwise = twoSum tail sum

f :: [Int] -> Int -> [(Int, Int)]
f l s = [(min a (s - a), max a (s - a)) | a <- l]

bothPositive :: (Int, Int) -> Bool
bothPositive (a, b) = (a >= 0) && (b >= 0)

firstRepeated :: Eq t => [t] -> t -> Maybe t
firstRepeated [] _ = Nothing
firstRepeated (head : tail) lastRead
  | head == lastRead = Just head
  | otherwise = firstRepeated tail head

twoSum2 :: [Int] -> Int -> Maybe (Int, Int)
twoSum2 l s = firstRepeated (sort (filter bothPositive (f l s))) (0, 0)

-- Part 2
threeSum :: [Int] -> Int -> Maybe (Int, Int, Int)
threeSum [] _ = Nothing
threeSum (head : tail) sum = do
  let twoSumResult = twoSum2 tail (sum - head)
  case twoSumResult of
    Just (n1, n2) -> Just (n1, n2, head)
    Nothing -> threeSum tail sum

main :: IO ()
main = do
  input <- getLine
  let numbers = read input :: [Int]
  -- Part 1
  let part1Results = twoSum2 numbers 2020
  case part1Results of
    Just (n1, n2) -> print $ n1 * n2
    Nothing -> putStrLn "Pt 1. Failed"

  -- Part 2
  let part2Results = threeSum numbers 2020
  case part2Results of
    Just (n1, n2, n3) -> print $ n1 * n2 * n3
    Nothing -> putStrLn "Pt 2. Failed"