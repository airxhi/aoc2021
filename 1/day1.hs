import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

testData :: [Int]
testData = [199,200,208,210,200,207,240,269,260,263]

numIncreases :: [Int] -> (Int, Int)
numIncreases = foldr (\x (a,b) -> (x, b + if a > x then 1 else 0)) (-1, 0)

checkWindow :: Int -> Int -> ([Int], Int) -> ([Int], Int)
checkWindow n x (window, acc)
    | length window < n = (newWindow, acc)
    | sum newWindow > sum window = (newWindow, acc+1)
    | otherwise = (newWindow, acc)
    where
        newWindow = take n (x:window)

numSlidingIncreases :: Int -> [Int] -> ([Int], Int)
numSlidingIncreases n = foldr (checkWindow n) ([], 0)

main :: IO ()
main = do
    inp <- fmap Text.lines (Text.readFile "input.txt")
    print . numIncreases . fmap (read . Text.unpack) $ inp
    print . numSlidingIncreases 3 . reverse . fmap (read . Text.unpack) $ inp