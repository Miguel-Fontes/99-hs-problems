-- https://wiki.haskell.org/99_questions/11_to_20

-- [11] --- Modified run-length encoding --------------------------------------------------------------------
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied ---
-- into the result list. Only elements with duplicates are transferred as (N E) lists. ----------------------
-- encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
data ListItem a = Multiple Int a | Single a deriving (Show)

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode [x] = (1, x) : encode []
encode (x:xs) = let reps = (+1) $ length $ takeWhile (==x) xs
                in  (reps, x) : encode (drop (reps-1) xs)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

-- [12] --- Decode a run-length encoded list -----------------------------------------------------------------
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version. ----
decode :: [(Int, a)] -> [a]
decode [(n, x)] = replicate n x
decode (x:xs) = replicate (fst x) (snd x) ++ decode xs

-- Pointfree
decode' :: [(Int, a)] -> [a]
decode'= foldr (\x acc -> replicate (fst x) (snd x) ++ acc) []

-- [13] --- Run-length encoding of a list (direct solution) --------------------------------------------------
{- Vai ficar por último porque não sei como criar novos tipos -}


-- [14] --- Duplicate the elements of a list -----------------------------------------------------------------
-- (dupli '(a b c c d)) => (A A B B C C C C D D)
dupl' :: [a] -> [a]
dupl' [] = []
dupl' (x:xs) = x:x:dupl' xs

-- [15] --- Replicate the elements of a list a given number of times -----------------------------------------
repli' :: Int -> [a] -> [a]
repli' _ [] = []
repli' n (x:xs) = replicate n x ++ repli' n xs


-- [16] --- Drop every N'th element from a list --------------------------------------------------------------
-- (drop '(a b c d e f g h i k) 3) => (A B D E G H K)
dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n x = take (n-1) x ++ dropEvery n (drop n x)

-- [17] ---  Split a list into two parts; the length of the first part is given ------------------------------
split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = take n xs : drop n xs : []


-- [18] --- Extract a slice from a list ----------------------------------------------------------------------
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th
-- element of the original list (both limits included). Start counting the elements with 1.
-- (slice '(a b c d e f g h i k) 3 7) => (C D E F G)
slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = []
slice i k xs | i > 0 = take (k-i+1) (drop (i-1) xs)


-- [19] --- Rotate a list N places to the left  --------------------------------------------------------------
-- (rotate '(a b c d e f g h) 3) => (D E F G H A B C)
-- (rotate '(a b c d e f g h) -2) => (G H A B C D E F)
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 x = x
rotate n xs
    | n > 0 = drop n $ xs ++ take n xs
    | n < 0 = drop j xs ++ take j xs
    where j = length xs - abs n -- Items que não serão modificados

-- [20] --- Remove the K'th element from a list  -------------------------------------------------------------
-- ?- removeAt 2 "abcd" => ('b',"acd")
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs
    | n > length xs = error "Index does not exists!"
    | n < length xs = (xs !! (n-1), take (n-1) xs ++ drop n xs)