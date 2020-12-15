listHas :: [Int] -> Int -> Bool
listHas [] _ = False
listHas (head : tail) num
  | head == num = True
  | otherwise = listHas tail num

twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum [] _ = Nothing
twoSum (head : tail) sum
  | listHas tail (sum - head) = Just (head, sum - head)
  | otherwise = twoSum tail sum

nHeadAndTail :: [t] -> Int -> [t] -> ([t], [t])
nHeadAndTail tail 0 head = (head, tail)
nHeadAndTail [] _ head = (head, [])
nHeadAndTail (h:tail) n head = nHeadAndTail tail (n-1) (head ++ [h])

findFirstInvalid :: [Int] -> [Int] -> Int
findFirstInvalid l (head:t)
    | twoSum l head == Nothing = head
    | otherwise = findFirstInvalid ((tail l) ++ [head]) t

findContiguousWithSum :: [Int] -> Int -> [Int] -> [Int]
findContiguousWithSum (h:t) s l
    | sum l < s = findContiguousWithSum t s (l ++ [h])
    | sum l > s = findContiguousWithSum (h:t) s (tail l)
    | sum l == s && length l > 1 = l
    | otherwise = findContiguousWithSum (h:t) s []

main :: IO ()
main = do
    input <- getLine
    let inputs = read input :: [Int]
    let (h, t) = nHeadAndTail inputs 25 []
    let firstInvalid = findFirstInvalid h t
    print $ firstInvalid
    let contiguousResult = findContiguousWithSum (h ++ t) firstInvalid []
    print $ foldr min 999999999 contiguousResult + foldr max 0 contiguousResult