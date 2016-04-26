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
rnd_permu x = x


-- [26] --- Generate the combinations of K distinct objects chosen from the N elements of a list ------------
data CombTree a = Node a [CombTree a] | Empty deriving (Show)

makeTree :: [a] -> CombTree [a]
makeTree xs = breakTree $ Node xs [Empty]

getV (Node v _) = v

breakTree :: CombTree [a] -> CombTree [a]
breakTree (Node xs [Empty]) = Node xs (map (\v -> Node [v] [Empty]) xs)

computeTree :: CombTree [a] -> CombTree [a]
computeTree (Node v (x:xs)) = makeTree sufixes
    where sufixes = filter (\a -> a /= getV x) v


-- https://en.wikipedia.org/wiki/Permutation