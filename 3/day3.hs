import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.Char (digitToInt)

testData :: [T.Text]
testData = map T.pack ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]

convertToInts :: [T.Text] -> [[Int]]
convertToInts = map (map digitToInt . T.unpack)

zeros :: [Int] -> [Int]
zeros xs = map (const 0) [0..(length xs)]

sumColumns :: [[Int]] -> [Int]
sumColumns xs = foldr (zipWith (+)) initial xs
    where initial = zeros (head xs)

binToInt :: [Bool] -> Int
binToInt = foldr (\x y -> fromEnum x + 2*y) 0 . reverse

invert :: [Bool] -> [Bool]
invert = map not

part1 :: [T.Text] -> Int
part1 xs = binToInt binary * binToInt (invert binary)
    where binary = map (\x -> x*2>=length xs) . sumColumns . convertToInts $ xs

mapToBool :: [Int] -> [Bool]
mapToBool = map (==1)

applyFiltering :: Int -> ([[Int]] -> Int -> Bool) -> [[Int]] -> [Int]
applyFiltering _ _ [x] = x
applyFiltering _ _ [] = [0]
applyFiltering i mostCommon xs = applyFiltering (i+1) mostCommon (filter (\x -> x!!i == val) xs)
    where
        val = fromEnum $ map (mostCommon xs) (sumColumns xs) !! i

part2 :: [T.Text] -> Int
part2 inp = oxygen * co2
    where
        pipeline mostCommon = binToInt . mapToBool . applyFiltering 0 mostCommon . convertToInts $ inp
        oxygen = pipeline (\xs x -> x*2 >= length xs)
        co2 = pipeline (\xs x -> x*2 < length xs)

main :: IO ()
main = do
    inp <- fmap T.lines (T.readFile "/home/alastair/programming/aoc/3/input.txt")
    print $ part1 inp
    print $ part2 inp