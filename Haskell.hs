asc :: Int -> Int -> [Int]
asc n m = aux n m []
    where
        aux n m acc
            | m >= n = aux n (m-1) (m:acc)
            | otherwise = acc


has :: [Int] -> Int -> Bool
has lst m
    | null lst = False
    | head lst == m = True
    | otherwise = has (tail lst) m

has2 :: (Eq a) => a -> [a] -> Bool
has2 _ [] = False
has2 e (x:xs) = (e == x) || (has2 e xs)

filt :: (Eq a) => [a] -> [a]
filt list = aux list []
    where 
        aux lst acc
            | null lst = acc
            | (head lst) `has2` (tail lst) = aux (tail lst) acc
            | otherwise = (head lst) : (aux (tail lst) acc)
            
filt2 :: (Eq a) => [a] -> [a]
filt2 [] = []
filt2 (x:xs)
    | x `has2` xs = filt2 xs
    | otherwise = x : filt2 xs
    
    
isAsc :: [Int] -> Bool
isAsc (x:xs)
    | null xs = True
    | x <= (head xs) = isAsc xs
    | otherwise = False

-- works only for from start node to any other node
-- hasPath :: [(Int, Int)] -> Int -> Int -> Bool

{-
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath graph src dest = dfs [] [src]
    where 
        dfs _ [] = False
        dfs visited (x:stack) = x == dest || dfs (x:visited) (neighbours visited x ++ stack)
        neighbours visited x = [e | (s, e)<-graph, s == x && not (elem e visited)]
-}


hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath graph src dest = dfs [] [src]
    where
        dfs _ [] = False
        dfs explored (current:unexplored)
            | current == dest = True
            | otherwise = dfs (current:explored) (neighbours explored current ++ unexplored)
                where neighbours explored current = [end | (start, end)<-graph, start == current && not (elem end explored)]


{- Max difference between two lists of strings -}
mxdiflg :: [String] -> [String] -> Maybe Int
mxdiflg s1 s2
  | null s1 || null s2 = Nothing
  | otherwise = if a > b then Just $ a else Just $ b
    where
      a = abs ( length (head (sbl s1)) - (length ( head (reverse (sbl s2)))))
      b = abs ( length (head (sbl s2)) - (length ( head (reverse (sbl s1)))))

-- Sort By Length
sbl :: [String] -> [String]
sbl strings = sortBy cmp strings
  where
    cmp :: String -> String -> Ordering
    cmp s1 s2 = compare (length s1) (length s2)



                
main :: IO ()
main = do
    let result = asc 1 3
    print result
    print (has result 4)
    print (has2 7 result)
    let fil = [1, 7, 1, 6, 9, 1, 3, 4, 6, 7, 1, 0, 1]
    print (filt fil)
    print(filt2 fil)
    let nums = [-1, -2, -3]
    print(isAsc nums)
    
    let graph = [(1, 2), (2, 3), (3, 2), (3, 4), (4, 3), (4, 5)]
    print (hasPath graph 1 6)


