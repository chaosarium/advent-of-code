import System.IO (readFile)
import Data.Set (Set)
import qualified Data.Set as Set

readInput :: String -> IO [Char]
readInput filename = do 
    input_chars :: [Char] <- readFile filename
    return input_chars

splitBy :: Char -> [Char] -> [[Char]]
splitBy sep cs = case break (\c -> c == sep) cs of
    (head, []) -> [head]
    (head, sep:rest) -> head : splitBy sep rest

parseInput :: [Char] -> [[Int]]
parseInput input_str = splitBy '\n' input_str |> filter (\line -> line /= []) |> map (\cs -> map (\c -> read [c]) cs)

printMat :: forall a. Show a => [[a]] -> IO ()
printMat = mapM_ print

-- ways to get to peak... turns out to not be what the question's asking oops
-- WAIT never mind this actually solves part 2? good thing we didn't delete the code :)
countTrailsWays :: [[Int]] -> Int -> [[Int]] -> [[Int]]
countTrailsWays topography base ascension = 
    let height = length topography in
    let width = length (topography !! 0) in
    zip [0..] topography |> 
    map (\(i, row) -> 
        zip [0..] row |>
        map (\(j, cell) -> 
            if cell /= base then 0 else (
                let up    = if i - 1 >= 0      then ascension !! (i-1) !! j else 0 in
                let down  = if i + 1 <  height then ascension !! (i+1) !! j else 0 in
                let left  = if j - 1 >= 0      then ascension !! i !! (j-1) else 0 in
                let right = if j + 1 <  width  then ascension !! i !! (j+1) else 0 in
                up + down + left + right
            )
        )
    )

-- part 1
collectTrails :: [[Int]] -> Int -> [[Set (Int, Int)]] -> [[Set (Int, Int)]]
collectTrails topography base ascension = 
    let height = length topography in
    let width = length (topography !! 0) in
    zip [0..] topography |> 
    map (\(i, row) -> 
        zip [0..] row |>
        map (\(j, cell) -> 
            if cell /= base then Set.empty else (
                let up    = if i - 1 >= 0      then ascension !! (i-1) !! j else Set.empty in
                let down  = if i + 1 <  height then ascension !! (i+1) !! j else Set.empty in
                let left  = if j - 1 >= 0      then ascension !! i !! (j-1) else Set.empty in
                let right = if j + 1 <  width  then ascension !! i !! (j+1) else Set.empty in
                Set.unions [up, down, left, right]
            )
        )
    )

(|>) :: a -> (a -> b) -> b
x |> f = f x

(<<|) :: (b -> c) -> (a -> b) -> (a -> c)
(g <<| f) x = g (f x)

nestedMap :: (a -> b) -> [[a]] -> [[b]]
nestedMap f = map (map f)

nestedEnumMap :: ((Int, Int, a) -> b) -> [[a]] -> [[b]]
nestedEnumMap f l = map (\(i, row) -> map (\(j, elem) -> f (i, j, elem)) (zip [0..] row) ) (zip [0..] l)

-- peak = peak height
solve2 :: [[Int]] -> Int -> Int -> [[Int]]
solve2 topography peak elevation = 
    if peak == elevation then nestedMap (\x -> if x == 9 then 1 else 0) topography
    else let ascension = solve2 topography peak (elevation+1) in 
    countTrailsWays topography elevation ascension

solve1 :: [[Int]] -> Int -> Int -> [[Set (Int, Int)]]
solve1 topography peak elevation = 
    if peak == elevation then nestedEnumMap (\(i, j, x) -> if x == 9 then Set.singleton (i, j) else Set.empty) topography
    else let ascension = solve1 topography peak (elevation+1) in 
    collectTrails topography elevation ascension
    
main :: IO ()
main = do
    p1_test_input <- readInput "test.txt"
    let topography = parseInput p1_test_input
    print "p1_test_input:"
    print (sum (concat (nestedMap Set.size (solve1 topography 9 0))))
    print "p2_test_input:"
    print (sum (concat (solve2 topography 9 0)))

    p1_real_input <- readInput "input.txt"
    let topography = parseInput p1_real_input
    print "p1_real_input:"
    print (sum (concat (nestedMap Set.size (solve1 topography 9 0))))
    print "p2_real_input:"
    print (sum (concat (solve2 topography 9 0)))
