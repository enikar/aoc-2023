-- Solution of AoC 2023, 3rd day
module Main where

import Data.Char (isDigit, digitToInt)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!?))

data Number = Number {number:: Int
                     ,line :: Int
                     ,xstart :: Int
                     ,xend :: Int
                     } deriving Show

main :: IO ()
main = do
  ldatas <- lines <$> readFile "day3.txt"
  let mdatas = buildMap ldatas
      numbers = buildNumbers ldatas
  showSolution "Part1" (part1 mdatas numbers)

part1 :: M.Map (Int, Int) Char -> [Number] -> Int
part1 grid = foldl' go 0
  where
    go acc num
      |nearSymbol grid num = acc + number num
      |otherwise           = acc

neighbours :: [(Int,Int)]
neighbours = [(-1,-1)
             ,(-1,0)
             ,(-1,1)
             ,(0,1)
             ,(0,-1)
             ,(1,-1)
             ,(1,0)
             ,(1,1)
             ]

addPair :: (Int, Int) -> (Int,Int) -> (Int,Int)
addPair (x,y) (x',y') = (x+x', y+y')

nearSymbol :: M.Map (Int, Int) Char -> Number -> Bool
nearSymbol grid num = any go positions
  where
    y0 = line num
    numPos = [(x, y0) | x <- [xstart num .. xend num]]
    positions :: [(Int, Int)]
    positions = addPair <$> numPos <*> neighbours

    go :: (Int, Int) -> Bool
    go p = case grid !? p of
             Nothing -> False
             (Just c) -> isSymbol c

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && (c /= '.')

buildMap :: [String] -> M.Map (Int, Int) Char
buildMap ls = M.fromList (zip inds s')
  where
    s' = concat ls
    ymax = length ls
    xmax = length (head ls)
    inds = [(x,y) | y <- [1..ymax], x <- [1..xmax]]

buildNumbers :: [String] -> [Number]
buildNumbers ls = concat (zipWith  buildNumbers' [1..] ls)

buildNumbers' :: Int -> String -> [Number]
buildNumbers' row line = finalize (foldr go ([],[]) (zip [1..] line))
  where
    finalize (numbers, digits)
      | null digits = numbers
      | otherwise   = readNumber digits : numbers

    go :: (Int, Char) -> ([Number], [Number]) -> ([Number], [Number])
    go (column, c) (numbers,digits)
      | isDigit c = (numbers, Number (digitToInt c) row column column : digits)
      | null digits = (numbers, [])
      | otherwise = (readNumber digits : numbers, [])


readNumber :: [Number] -> Number
readNumber ds = foldl' go (head ds) (tail ds)
  where go (Number n0 y0 xs0 _) (Number n1 y1 xs1 _)
          | y0 /= y1 = error "Error: readNumber: line numbers don't match"
          |otherwise = Number (n0 * 10 + n1) y0 xs0 xs1

showSolution :: Show a => String -> a -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)
