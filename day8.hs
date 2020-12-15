executeCode :: [(String, Int, Bool)] -> (String, Int, Bool) -> [(String, Int, Bool)] -> Int -> Int
executeCode _ (_, _, True) _ acc = acc
executeCode _ (op, val, _) [] acc
    | op == "nop" = acc
    | op == "acc" = (acc+val)
executeCode h (op, val, _) t acc
    | op == "nop" = executeCode (h ++ [(op, val, True)]) (head (tail t)) (tail t) acc
    | op == "acc" = executeCode (h ++ [(op, val, True)]) (head (tail t)) (tail t) (acc+val)
    | op == "jmp" && val > 0 = executeCode (h ++ [head t]) (op, val-1, False) (tail t) acc
    | op == "jmp" && val < 0 = executeCode (init h) (op, val+1, False) (last h:t) acc
    | op == "jmp" && val == 0 = executeCode h (head t) t acc

skip :: Int -> String -> String
skip 0 s = s
skip n (_:t) = skip (n-1) t

removeLeadingPlus :: String -> String
removeLeadingPlus (h:t)
    | h == '+' = t
    | otherwise = h:t

parseOp :: String -> (String, Int, Bool)
parseOp s = (take 3 s, read (removeLeadingPlus (skip 4 s)) :: Int, False)

parseInput :: [String] -> [(String, Int, Bool)] -> [(String, Int, Bool)]
parseInput [] parsed = parsed
parseInput (h:t) parsed = parseInput t (parsed ++ [parseOp h])

main :: IO ()
main = do
    input <- getLine
    let opStrs = read input :: [String]
    let ops = parseInput opStrs []
    print $ executeCode [] (head ops) ops 0