-- hmmmm no idea what lazy + immutable array means in terms of performance
-- surely haskell is call by need right?...
-- but does it materialize the whole array upon need or just the individual cells needed?
-- meh whatever ╮(￣▽￣)╭

-- wait it's boxes that we're pushing around? i thought we're pushing lanternfish
-- ehh whatever what's the difference between boxes and lanternfish anyway

import System.IO (readFile)
import Data.List
import Data.Array

readInput :: String -> IO String
readInput filename = do 
    input_chars :: String <- readFile filename
    return input_chars

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

data Object = Empty | Wall | Lanternfish | Submarine | Open | Close    deriving (Show, Eq)
data Direction = U | D | L | R    deriving (Show, Eq)
type Map = Array (Int, Int) Object
type Coord = (Int, Int)

parseMap :: String -> Map
parseMap map_string = 
    let lines = splitBy '\n' map_string |> filter (\line -> line /= []) in
    let height = length lines in
    let width = length (head lines) in
    map (\cs -> map (\c -> case c of 
            '.' -> Empty
            '#' -> Wall
            'O' -> Lanternfish
            '@' -> Submarine
        ) cs) lines
    |> concat
    |> listArray ((0, 0), (height  - 1 , width - 1))
    
-- part 2
parseMap2 :: String -> Map
parseMap2 map_string = 
    let lines = splitBy '\n' map_string |> filter (\line -> line /= []) in
    let height = length lines in
    let width = 2 * (length (head lines)) in
    map (\cs -> map (\c -> case c of 
            '.' -> [Empty, Empty]
            '#' -> [Wall, Wall]
            'O' -> [Open, Close]
            '@' -> [Submarine, Empty]
        ) cs |> concat) lines
    |> concat
    |> listArray ((0, 0), (height - 1, width - 1))


parseControl :: String -> [Direction]
parseControl control_string = filter (\c -> c /= '\n') control_string
    |> map (\c -> case c of
        '<' -> L
        '>' -> R
        '^' -> U
        'v' -> D
    )
    
findSubmarine :: Map -> Coord
findSubmarine m = 
    let is = indices m in
    let es = elems m in
    zip is es |> filter (\((i, j), e) -> case e of 
        Submarine -> True 
        _ -> False
    ) |> head |> fst

movedCoord :: Coord -> Direction -> Coord
movedCoord (i, j) d = case d of 
    U -> (i-1, j)
    D -> (i+1, j)
    L -> (i, j-1)
    R -> (i, j+1)

-- move thing at c in direction, overwritting new location c' and leaving c empty
makeMove :: Map -> Coord -> Direction -> Map
makeMove m c d = 
    let c' = movedCoord c d in
    m // [(c', m ! c), (c, Empty)]
    
tryMove :: Map -> Coord -> Direction -> Maybe Map
tryMove m c@(i, j) d =
    let target_coord = movedCoord c d in
    case m ! target_coord of
        Empty -> Just (makeMove m c d)
        Wall -> Nothing
        Lanternfish -> (tryMove m target_coord d) >>= (\m' -> Just (makeMove m' c d))

-- part 2
tryMove2 :: Map -> Coord -> Direction -> Maybe Map
tryMove2 m c@(i, j) d =
    let target_coord = movedCoord c d in
    case (d, m ! target_coord) of
        (_, Empty) -> Just (makeMove m c d)
        (_, Wall) -> Nothing
        (d, target_cell) | ((d == L || d == R) && (target_cell == Open || target_cell == Close)) -> 
            (tryMove2 m target_coord d) >>= (\m' -> Just (makeMove m' c d))
        (d, target_cell) | ((d == U || d == D) && (target_cell == Open || target_cell == Close)) -> 
            let matched_paren_side = if target_cell == Open then R else L in
            let close_coord = movedCoord target_coord matched_paren_side in
            (tryMove2 m target_coord d) >>= (\m' -> tryMove2 m' close_coord d) >>= (\m'' -> Just (makeMove m'' c d))

executeMoves :: (Map -> Coord -> Direction -> Maybe Map) -> Map -> Coord -> [Direction] -> Map
executeMoves movetryer m c directions =
    case directions of 
        [] -> m
        d:ds -> 
            let movedopt = movetryer m c d in
            let (m', c') = case movedopt of 
                    Nothing -> (m, c)
                    Just m' -> (m', movedCoord c d)
            in
                executeMoves movetryer m' c' ds 

gpsCoord :: Coord -> Int
gpsCoord (i, j) = i* 100 + j

-- sum up coord of all boxes
fishCoords :: Map -> Int
fishCoords m = 
    let is = indices m in
    let es = elems m in
    zip is es |> filter (\((i, j), e) -> case e of 
        Lanternfish -> True 
        Open -> True 
        _ -> False
    ) 
    |> map fst
    |> map gpsCoord
    |> sum

printMat :: forall a. Show a => [[a]] -> IO ()
printMat = mapM_ print

printList :: forall a. Show a => [a] -> IO ()
printList = mapM_ print

(|>) :: a -> (a -> b) -> b
x |> f = f x

nestedMap :: (a -> b) -> [[a]] -> [[b]]
nestedMap f = map (map f)

nestedEnumMap :: ((Int, Int, a) -> b) -> [[a]] -> [[b]]
nestedEnumMap f l = map (\(i, row) -> map (\(j, elem) -> f (i, j, elem)) (zip [0..] row) ) (zip [0..] l)
    
part1 :: String -> IO ()
part1 file_name = do
    input_str <- readInput file_name
    let map_str:control_str:[] = (breakBySubstr "\n\n" input_str)
    let m = parseMap map_str
    let controls = parseControl control_str
    let init_submarine_coord = findSubmarine m
    print ("part1 " ++ file_name ++ ":")
    print (executeMoves tryMove m init_submarine_coord controls |> fishCoords)

part2 :: String -> IO ()
part2 file_name = do
    input_str <- readInput file_name
    let map_str:control_str:[] = (breakBySubstr "\n\n" input_str)
    let m = parseMap2 map_str
    let controls = parseControl control_str
    let init_submarine_coord = findSubmarine m
    let m_final = executeMoves tryMove2 m init_submarine_coord controls
    print ("part2 " ++ file_name ++ ":")
    print (m_final |> fishCoords)

main :: IO ()
main = do
    part1 "test.txt"
    part1 "test2.txt"
    part1 "input.txt"
    
    part2 "p2test1.txt"
    part2 "test.txt"
    part2 "test2.txt"
    part2 "input.txt"