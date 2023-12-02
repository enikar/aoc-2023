-- Solution of AoC, 1st day
module Main where

import Data.List (foldl', isPrefixOf, tails)
import Data.Char (digitToInt, isDigit)
import qualified Data.Map.Strict as M
import Data.Map (Map)

main :: IO ()
main = do
  datas <- lines <$> readFile "day1.txt"
  showSolution "Part1" (partx lineToInt1 datas)
  showSolution "Part2" (partx lineToInt2 datas)


showSolution :: Show a => String -> a -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)


partx :: (String -> Int) -> [String] -> Int
partx f = foldl' go 0
  where
    go acc s = acc + f s

lineToInt1 :: String -> Int
lineToInt1 s = start * 10 + end
  where start = head l
        end = last l
        l = map digitToInt (filter isDigit s)

digits :: Map String Int
digits = M.fromList [("one", 1)
                    ,("1", 1)
                    ,("two", 2)
                    ,("2", 2)
                    ,("three", 3)
                    ,("3", 3)
                    ,("four", 4)
                    ,("4", 4)
                    ,("five", 5)
                    ,("5", 5)
                    ,("six", 6)
                    ,("6", 6)
                    ,("seven", 7)
                    ,("7", 7)
                    ,("eight", 8)
                    ,("8", 8)
                    ,("nine", 9)
                    ,("9", 9)
                    ]

digitKeys :: [String]
digitKeys = M.keys digits

-- Idea: use isPrefixOf and tails to fold the string to a list of digits
lineToInt2 :: String -> Int
lineToInt2 ss = 10 * start + end
  where
    start = head l
    end = last l
    l = foldr go [] (tails ss)
    go s acc = case lookupDigit s of
                 (Just x) -> x:acc
                 Nothing  -> acc

lookupDigit :: String -> Maybe Int
lookupDigit s = foldl' go Nothing digitKeys
  where
    go (Just x) _ = Just x
    go Nothing key = if key `isPrefixOf` s
                     then M.lookup key digits
                     else Nothing
