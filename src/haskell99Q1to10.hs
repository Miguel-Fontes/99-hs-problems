-- Exercícios https://wiki.haskell.org/99_questions/1_to_10

-- [1] --- Find the last element of a list -----------------------------------------------------------------
myLast :: [a] -> a;
myLast [] = error "Treta";
myLast [x] = x;
myLast (x:xs) = myLast xs;

-- [2] --- Find the last but one element of a list ----------------------------------------------------------
myButLast :: [a] -> a
myButLast = last . tail

-- [3] Find the K'th element of a list. The first element in the list is number 1 ---------------------------
myGetElement :: Int -> [a] -> a
myGetElement n y  = myLast $ take n y

myGetElementTest :: String
myGetElementTest
    | myGetElement 3 [1,2,3,4,5] == 3 = "PASS - Elemento 3 da lista " ++ show lista ++ " e 3"
    | otherwise = "FAIL - Element 3 da lista não é 3"
    where lista = [1,2,3,4,5]; elemento = 3;

-- [4] --- Find the number of elements of a list ------------------------------------------------------------
myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc + 1 ) 0

-- [5] --- Reverse a list -----------------------------------------------------------------------------------
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

{- Com recursão just for the funz => utilizando join dizem ser mais lento. Vdd, Arnaldo? -}
myReverse' :: [a] -> [a]
myReverse' x = case x of [] -> []
                         (x:xs) -> myReverse' xs ++ [x]

-- Input  => [1,2,3,4,5] ------------------------------------------------------------------------------------
-- x = 1 -> xs = [2,3,4,5]  -> result = [5,2,3,4] ++ [1] => [5,4,3,2,1]
-- x = 2 -> xs = [3,4,5]    -> result = [5,3, 4]  ++ [2] => [5,4,3,2
-- x = 3 -> xs = [4,5]      -> result = [5,4]     ++ [3] => [5,4,3]
-- x = 4 -> xs = [5]        -> result = [5]       ++ [4] => [5,4]
-- x = 5 -> xs = []         -> result = []        ++ [5] => [5]
-- Output => [5,4,3,2,1] ------------------------------------------------------------------------------------

-- [6] Find out whether a list is a palindrome. A palindrome can be read forward or backward; ---------------
-- e.g. (x a m a x). ----------------------------------------------------------------------------------------
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome x = x == myReverse x

-- [7] Flatten a nested list structure ----------------------------------------------------------------------
-- * (my-flatten '(a (b (c d) e))) => (A B C D E)
myFlatten :: [[a]] -> [a]
myFlatten [] = []
myFlatten [x] = x
myFlatten (x:xs) = x ++ myFlatten xs

-- [8] Eliminate consecutive duplicates of list elements. ---------------------------------------------------
-- If a list contains repeated elements they should be replaced with a single copy of the
-- element. The order of the elements should not be changed.
myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x:xs)
    | x == head xs = myCompress xs
    | otherwise = x : myCompress xs


-- [9] Pack consecutive duplicates of list elements into sublists -------------------------------------------
-- If a list contains repeated elements they should be placed in separate sublists --------------------------
pack :: (Eq a) => [a] -> [[a]] -- [1,1,1,2,2,2,5,4,5,4] => [[1,1,1], [2,2,2], [5,5], [4,4]]
pack [] = []
pack [x] = [[x]]
pack (x:xs)
   | repetitions > 0 = replicate repetitions x : pack filtered
   | otherwise = pack filtered
   where repetitions = (+1) $ length $ filter (==x) xs
         filtered = filter (/=x) xs

-- Input  => [1,1,1,2,2,2,5,4,5,4] --------------------------------------------------------------------------
-- x = 1   -> xs = [1,1,2,2,2,5,4,5,4] -> filtered = [2,2,2,5,4,5,4] -> repetitions = 3 -> result = [1,1,1]
-- x = 2   -> xs = [2,2,5,4,5,4]       -> filtered = [5,4,5,4]       -> repetitions = 3 -> result = [2,2,2]
-- x = 5   -> xs = [4,5,4]             -> filtered = [4,4]           -> repetitions = 2 -> result = [5,5]
-- x = 4   -> xs = [4]                 -> filtered = []              -> repetitions = 2 -> result = [4,4]
-- Output => [[1,1,1], [2,2,2], [5,5], [4,4]] ---------------------------------------------------------------


-- [10]  Run-length encoding of a list ------------------------------------------------------------------------
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the
-- element E. e.g. (encode '(a a a a b c c a a d e e e e)) => ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
encode :: [[a]] -> [(Int, a)]
encode [x] = [(length x, head x)]
encode (x:xs) = (length x, head x) : encode xs

