import System.IO (readFile)
import Data.List
import Data.Array
import qualified Data.Set as Set

readInput :: String -> IO String
readInput filename = do 
    input_str :: String <- readFile filename
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

data Object = Key (Array Int Int) | Lock (Array Int Int)    deriving (Show, Eq)

parseOne :: String -> Object
parseOne object_str = 
    let 
        lines = splitBy '\n' object_str 
            |> filter (\line -> line /= []) 
            |> concat
            |> listArray ((0, 0), (6, 4))
    in
    let 
        col_heights = [0,1,2,3,4]
            |> map (\col_idx -> [1,2,3,4,5] 
                |> map (\row_idx -> if lines ! (row_idx, col_idx) == '#' then 1 else 0) 
                |> sum) 
                |> listArray (0, 4)
    in    
    if (lines ! (0, 0)) == '#' then 
        Lock col_heights 
    else 
        Key col_heights

(|>) :: a -> (a -> b) -> b
x |> f = f x

enum = zip [0..]

part1 :: String -> IO ()
part1 file_name = do
    input_str <- readInput file_name
    let keys_and_locks_strs = breakBySubstr "\n\n" input_str
    let objects = map parseOne keys_and_locks_strs
    let key_height_sets_init = [0,1,2,3,4] |> map (\i -> [0,1,2,3,4,5] |> map (\j -> Set.empty)) |> concat |> listArray ((0, 0), (4, 5))
    let key_height_sets = objects 
            |> enum
            |> filter (\(idx, obj) -> case obj of 
                Key _ -> True 
                _ -> False
            )
            |> foldr (\(idx, obj) -> \acc -> 
                let Key heights = obj in
                [0,1,2,3,4]
                |> foldr (\j -> \acc' -> acc' // [(
                    (j, heights ! j), 
                    Set.union (acc' ! (j, heights ! j)) (Set.singleton idx)  
                )]) acc
            ) key_height_sets_init
    let key_count_per_lock = objects 
            |> enum
            |> filter (\(idx, obj) -> case obj of 
                Lock _ -> True 
                _ -> False
            )
            |> map (\(idx, obj) -> 
                let Lock heights = obj in
                [0,1,2,3,4]
                |> map (\j -> 
                    let lock_height = heights ! j in
                    [0,1,2,3,4,5] 
                    |> filter (\key_height -> lock_height + key_height <= 5)
                    |> map (\key_height -> key_height_sets ! (j, key_height))
                    |> Set.unions
                )
                |> foldl1 Set.intersection
            )
    let answer = key_count_per_lock |> map Set.size |> sum
    print ("part1 " ++ file_name ++ ": " ++ show answer)

main :: IO ()
main = do
    part1 "test.txt"
    part1 "input.txt"