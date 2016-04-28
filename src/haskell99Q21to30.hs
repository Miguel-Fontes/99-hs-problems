-- https://wiki.haskell.org/99_questions/21_to_28
import System.Random

-- [21] --- Insert an element at a given position into a list -----------------------------------------------
-- insertAt 'X' "abcd" 2 => "aXbcd"
insertAt :: Int -> [a] -> a -> [a]
insertAt i xs x = take index xs ++ [x] ++ drop index xs
    where index = i - 1


-- [22] --- Create a list containing all integers within a given range --------------------------------------
-- range 4 9 => [4,5,6,7,8,9]
range :: Int -> Int -> [Int]
range x y
    | y > x = [x..y]
    | otherwise = [x, x-1..y]

-- [23] --- Extract a given number of randomly selected elements from a list --------------------------------
-- rnd_select "abcdefgh" 3 >>= putStrLn => eda
rnd_select :: (Ord a) => [a] -> Int -> [a]
rnd_select _ 0 = []
rnd_select x y = let rad = fst (randomR (bottom, top) (mkStdGen (y*500000)) :: (Int, StdGen))
                     top = (length x) - 1
                     bottom = 1
                 in  x !! rad : rnd_select x (y-1)


-- [24] --- Lotto: Draw N different random numbers from the set 1..M ----------------------------------------
-- diff_select 6 49 => [23,1,17,33,21,37]
diff_select :: Int -> Int -> [Int]
diff_select 0 _ = []
diff_select x y = let rad = fst (randomR (1 , y) (mkStdGen (x*10000*y)) :: (Int, StdGen))
                  in  rad : diff_select (x-1) y

-- [25] --- Generate a random permutation of the elements of a list -----------------------------------------
-- rnd_permu "abcdef" => "badcef"
rnd_permu :: [a] -> [a]
rnd_permu x = undefined


-- [26] --- Generate the combinations of K distinct objects chosen from the N elements of a list ------------
-- Combinations
combinationsNo :: (Ord a, Fractional a) => a -> a -> a
combinationsNo n r = factorial n / (factorial r * factorial(n - r))

-- n = 2 xs = [1,2,3]
-- [[1,2,3] !! i : y | i <- [0], y <- [[2], [3]] ] => [1,2] : [1,3]
-- [[1,2,3] !! i : y | i <- [1], y <- [[3]] ] => [2,3]
-- [[1,2,3] !! i : y | i <- [2], y <- [] ] => []
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                                  , x <- combinations (n-1) (drop (i+1) xs) ]


-- Permutations
permutation :: Eq a => Int -> [a] -> [[a]]
permutation r ns
    | r == 0 = [ns]
    | r == 1 = breakList ns
    | r > length ns = [ns]
    | otherwise =  permutationIter r (breakList ns)
    where permutationIter r' ds
              | r' > 1 = permutationIter (r' - 1) (concat $ map (permute ns) ds)
              | otherwise = ds

permute :: Eq a => [a] -> [a] -> [[a]]
permute ds xs = foldr step [] uniqueDs
    where step d acc = (xs ++ [d]) : acc
          uniqueDs = filter (\x -> not (x `elem` xs)) ds

breakList :: [a] -> [[a]]
breakList xs = map toSublist xs
    where toSublist x = [x]

-- Validate our result
permutationsNo :: (Ord a, Fractional a) => a -> a -> a
permutationsNo n r = (factorial n) / (factorial (n-r))

factorial :: (Num a, Ord a) => a -> a
factorial x = factIter 1 1 x
    where factIter a b n
              | n > 0 = factIter (a * b) (b+1) (n-1)
              | otherwise = a
