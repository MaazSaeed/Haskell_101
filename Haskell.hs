import Data.Char
import Data.List

t :: Bool -> Bool -> Bool
t True True = True
t _ _ = False

firsts :: [(a, b)] -> [a]
firsts xs = [x | (x, _) <- xs]

lengthy :: [a] -> Int
lengthy xs = sum [1 | _ <- xs]

prime :: Int -> Bool
prime n = aux 2 (n `div` 2)
    where
        aux i e
            | i > e          = True
            | n `mod` i == 0 = False
            | otherwise      = aux (i + 1) e

factors :: Int -> [Int]
factors n = [x | x <- [2..n], prime x]

let2int :: Char -> Int
let2int x = ord x - ord 'a'

int2let :: Int -> Char
int2let x = chr ( ord 'a' + x ) 

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ( (let2int c + n) `mod` 26 )
    | otherwise = c
    
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

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
                where 
                    neighbours explored current = [end | (start, end)<-graph, start == current && not (elem end explored)]


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



{-
    Shunting Yard Algorithm
    fixes needed
-}

toPostfix :: String -> String
toPostfix str = aux str [] []
  where
    aux str_ stack queue
      | null str_ = emptyStack stack queue
      | isDigit_ ch = aux (tail str_) (stack) (queue ++ [ch])
      | isOperator ch = aux (tail str_) (stack ++ fst sq) (queue ++ snd sq)
      | isExpo ch = aux (tail str_) (stack ++ [ch]) queue
      | isLeftParen ch = aux (tail str_) (stack ++ [ch]) queue
      | isRightParen ch = aux (tail str_) (fst sqP) (queue ++ snd sqP)
      | otherwise = queue
      where
        ch = head str_
        isDigit_ c = c >= '0' && c <= '9'
        isOperator c = c == '+' || c == '-' || c == '*' || c == '/'
        isExpo c = c == '^'
        isLeftParen c = c == '('
        isRightParen c = c == ')'
        prec c = aux c
          where
            aux c
              | c == '+' || c == '-' = 1
              | c == '*' || c == '/' = 2
              | c == '^' = 3
              | ch == '(' = 0
              | otherwise = -1
        updateSQ stk c = aux (reverse stk) c ([], [])
          where
            aux s c acc
              | null s = ([c] ++ fst acc, snd acc)
              | prec (head s) >= prec c = aux (tail s) c (tail s, (snd acc) ++ [head s])
              | otherwise = ([c] ++ fst acc, snd acc)
        updateSQP stk = aux (reverse stk) ([], [])
          where
            aux s acc
              | null s = (tail s, snd acc)
              | head s /= '(' = aux (tail s) (tail s, (snd acc) ++ [head s])
              | otherwise = (tail s, snd acc)
        sq = updateSQ stack ch 
        sqP = updateSQP stack
        emptyStack s q = aux (reverse s) q
          where
            aux s_ q_
              | null s_ = q_
              | otherwise = aux (tail s_) (q_ ++ [head s_ ] )
          

          



                
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



    print (firsts [(1, 'a'), (2, 'b'), (3, 'c')])
    print (lengthy [1, 2, 3, 4, 5, 6])
    print (factors 100)
    print (encode 3 "haskell is fun")
    print (encode (-3) "kdvnhoo lv ixq" )
    
    
    
    let graph = [(1, 2), (2, 3), (3, 2), (3, 4), (4, 3), (4, 5)]
    print (hasPath graph 1 6)


