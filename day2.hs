{-
    Parsing
-}
parseInputLine :: String -> (Int, Int, Char, String)
parseInputLine s = do
  let minStr = getStrUntil s '-' ""
  let maxStr = getStrUntil (skipNChars s ((length minStr) + 1)) ' ' ""
  let char = head (skipNChars s ((length minStr) + (length maxStr) + 2))
  let password = skipNChars s ((length minStr) + (length maxStr) + 5)
  (read minStr :: Int, read maxStr :: Int, char, password)

skipNChars :: String -> Int -> String
skipNChars s 0 = s
skipNChars (_ : tail) n = skipNChars tail (n -1)

getStrUntil :: String -> Char -> String -> String
getStrUntil (head : tail) endChar numStr
  | head == endChar = numStr
  | otherwise = getStrUntil tail endChar (numStr ++ [head])

{-
    Check input
-}

-- Part 1
countCharInStr :: String -> Char -> Int -> Int
countCharInStr [] _ count = count
countCharInStr (head : tail) c count
  | head == c = countCharInStr tail c (count + 1)
  | otherwise = countCharInStr tail c count

passwordIsValid :: (Int, Int, Char, String) -> Bool
passwordIsValid (minCount, maxCount, c, password) = do
  let cCount = countCharInStr password c 0
  ((cCount >= minCount) && (cCount <= maxCount))

--Part 2
passwordIsValid2 :: (Int, Int, Char, String) -> Bool
passwordIsValid2 (i, j, c, password)
  | password !! (i -1) == password !! (j -1) = False
  | password !! (i -1) == c || password !! (j -1) == c = True
  | otherwise = False

{-
    Shenanigans
-}

boolToInt :: Bool -> Int
boolToInt b
  | b = 1
  | otherwise = 0

{-
    Main
-}
main :: IO ()
main = do
  input <- getLine
  let inputsStr = read input :: [String]
  let inputs = map parseInputLine inputsStr
  print $ sum $ map (boolToInt . passwordIsValid) inputs
  print $ sum $ map (boolToInt . passwordIsValid2) inputs