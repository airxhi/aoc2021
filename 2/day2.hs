import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Fixed as T

testData :: [T.Text]
testData = fmap T.pack ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

tuplify2 :: [T.Text] -> (T.Text, Int)
tuplify2 [a, b] = (a, read . T.unpack $ b)
tuplify2 xs = (T.empty, 0)

tokenize :: T.Text -> (T.Text, Int)
tokenize = tuplify2 <$> T.splitOn (T.pack " ")

move :: (T.Text, Int) -> (Int, Int) -> (Int, Int)
move (direction, amount) (x,y)
    | direction == T.pack "forward" = (x + amount, y)
    | direction == T.pack "up" = (x, y - amount)
    | direction == T.pack "down" = (x, y + amount)
    | otherwise = (x,y)

move2 :: (T.Text, Int) -> (Int, Int, Int) -> (Int, Int, Int)
move2 (direction, amount) (x,y,aim)
    | direction == T.pack "forward" = (x + amount, y + amount * aim, aim)
    | direction == T.pack "up" = (x, y, aim - amount)
    | direction == T.pack "down" = (x, y, aim + amount)
    | otherwise = (x,y,aim)

mult2Tuple :: Num a => (a, a) -> a
mult2Tuple (a,b) = a*b

first2 :: (a, b, c) -> (a, b)
first2 (a,b,c) = (a,b)

part1 :: [T.Text] -> Int
part1 = mult2Tuple . foldr move (0,0) . fmap tokenize . reverse

part2 :: [T.Text] -> Int
part2 = mult2Tuple . first2 . foldr move2 (0,0,0) . fmap tokenize . reverse

main :: IO ()
main = do
    inp <- getContents "input.txt"
    print $ part1 inp
    print $ part2 inp