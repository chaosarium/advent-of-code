module Utils (
    readInput,
    splitBy,
    breakBySubstr,
    printMat,
    printList,
    (|>),
    nestedMap,
    nestedEnumMap,
    parseGrid,
    findFirstInGrid,
    oneTowardInCoord,
    Grid,
    Coord,
    Direction,
) where

import Data.List
import Data.Array


type Coord = (Int, Int)
data Direction = North | South | West | East    deriving (Show, Eq)

-- probably helps parsing

readInput :: String -> IO String
readInput filename = do 
    input_str <- readFile filename
    return input_str

splitBy :: Char -> [Char] -> [[Char]]
splitBy sep cs = case break (\c -> c == sep) cs of
    (head, []) -> [head]
    (head, sep:rest) -> head : splitBy sep rest

breakBySubstr' :: [Char] -> [Char] -> [Char] -> [[Char]]
breakBySubstr' sep s prefix = if isPrefixOf sep s then
    let s' = drop (length sep) s in prefix:(breakBySubstr' sep s' [])
    else case s of 
        [] -> [prefix]
        c:s' -> case breakBySubstr' sep s' prefix of 
            [] -> [prefix]
            h:rs -> (c:h):rs

breakBySubstr :: [Char] -> [Char] -> [[Char]]
breakBySubstr sep s = breakBySubstr' sep s []

-- grid parsing

type Grid a = Array (Int, Int) a
-- turns sth like
-- abc
-- def
-- ghi
-- into matrix
parseGrid :: (Char -> a) -> String -> Grid a
parseGrid char2elem grid_str = 
    let lines = Utils.splitBy '\n' grid_str |> filter (\line -> line /= []) in
    let height = length lines in
    let width = length (head lines) in
    map (\cs -> map char2elem cs) lines
    |> concat
    |> listArray ((0, 0), (height  - 1 , width - 1))

findFirstInGrid :: (a -> Bool) -> Grid a -> Coord
findFirstInGrid p m = 
    let idxs = indices m in
    let elements = elems m in
    zip idxs elements |> filter (\((i, j), e) -> p e) |> head |> fst

-- coordinate after taking one cell in direction
oneTowardInCoord :: Coord -> Direction -> Coord
oneTowardInCoord (i, j) d = case d of 
    North -> (i-1, j)
    South -> (i+1, j)
    West -> (i, j-1)
    East -> (i, j+1)

    
-- list parsing

-- maps each char to some alpha, ignoring new lines
parseCharSeq :: (Char -> a) -> String -> [a]
parseCharSeq char2elem char_seq = filter (\c -> c /= '\n') char_seq
    |> map char2elem


-- printing

printMat :: Show a => [[a]] -> IO ()
printMat = mapM_ print

printList :: Show a => [a] -> IO ()
printList = mapM_ print

-- hofs

(|>) :: a -> (a -> b) -> b
x |> f = f x

nestedMap :: (a -> b) -> [[a]] -> [[b]]
nestedMap f = map (map f)

nestedEnumMap :: ((Int, Int, a) -> b) -> [[a]] -> [[b]]
nestedEnumMap f l = map (\(i, row) -> map (\(j, elem) -> f (i, j, elem)) (zip [0..] row) ) (zip [0..] l)

