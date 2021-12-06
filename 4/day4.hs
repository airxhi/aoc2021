import System.IO
import qualified Data.Set as Set
import Data.List (transpose)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)

type Row = [Int]

readInt :: String -> Int
readInt = read

evaluateBoard :: Set.Set Int -> [Row] -> Bool
evaluateBoard drawn board = any (all (`Set.member` drawn)) (board ++ transpose board)

findWinner :: [[Row]] -> Set.Set Int -> Int -> Int
findWinner boards drawn lastDrawn = (*lastDrawn) . sum . filter (\x -> not (Set.member x drawn)) . concat . head . filter (evaluateBoard drawn) $ boards

evaluateDraws :: [[Row]] -> [Int] -> Set.Set Int -> Int
evaluateDraws _ [] _ = 0
evaluateDraws boards (d:draws) drawn 
    | any (evaluateBoard (Set.insert d drawn)) boards = findWinner boards (Set.insert d drawn) d
    | otherwise = evaluateDraws boards draws (Set.insert d drawn)

filterBoards :: [[Row]] -> [Int] -> Set.Set Int -> Int
filterBoards _ [] _ = 0
filterBoards [board] (d:draws) drawn 
    | evaluateBoard (Set.insert d drawn) board = findWinner [board]  (Set.insert d drawn) d
    | otherwise = filterBoards [board] draws (Set.insert d drawn)
filterBoards boards (d:draws) drawn = filterBoards (filter (not . evaluateBoard (Set.insert d drawn)) boards) draws (Set.insert d drawn)

createBoards :: [String] -> [[Row]]
createBoards lines = map createBoard (partition 5 (tail lines))
    where 
        createRow = map readInt . filter (/="") . words
        createBoard = map createRow


part1 :: String -> Int
part1 xs = evaluateDraws boards draws (Set.empty :: Set.Set Int) 
    where
        lines = wordsWhen (=='\n') xs
        draws = map readInt . wordsWhen (==',') $ head lines
        boards = createBoards lines

part2 :: String -> Int
part2 xs = filterBoards boards draws (Set.empty :: Set.Set Int) 
    where
        lines = wordsWhen (=='\n') xs
        draws = map readInt . wordsWhen (==',') $ head lines
        boards = createBoards lines


main :: IO ()
main = do
    handle <- openFile "/home/alastair/programming/aoc/4/input.txt" ReadMode
    contents <- hGetContents handle
    print $ part1 contents
    print $ part2 contents
    hClose handle