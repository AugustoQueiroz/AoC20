earliestToTakeBus :: Int -> Int -> Int
earliestToTakeBus lo bus = lo + bus - (lo `mod` bus)

findEarliestBusTime :: [Int] -> Int -> Int
findEarliestBusTime buses lo = foldr min 99999999999999 (map (earliestToTakeBus lo) buses)

busArrivesAtTime :: Int -> Int -> Bool
busArrivesAtTime time bus = time `mod` bus == 0

findBusThatArrivesAt :: [Int] -> Int -> Int
findBusThatArrivesAt buses time = head $ filter (busArrivesAtTime time) buses

parseBuses :: String -> String -> [Int] -> [Int]
parseBuses "" s result = result ++ [read s :: Int]
parseBuses (',':t) rollingS result = parseBuses t "" (result ++ [read rollingS :: Int])
parseBuses ('x':t) _ result = parseBuses (tail t) "" result
parseBuses (h:t) rollingS result = parseBuses t (rollingS ++ [h]) result

main :: IO ()
main = do
    a <- getLine
    b <- getLine
    let lo = read a :: Int
    let buses = parseBuses b "" []
    let departTime = findEarliestBusTime buses lo
    print $ (departTime - lo) * (findBusThatArrivesAt buses departTime)