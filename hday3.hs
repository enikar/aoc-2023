-- Solution of AoC 2023, 3rd day
module Main where

import Data.Char (isDigit, digitToInt)
import Data.List (foldl', find, nub)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!?))

data Number = Number {number:: Int
                     ,line :: Int
                     ,xstart :: Int
                     ,xend :: Int
                     } deriving (Show, Eq)

main :: IO ()
main = do
  datas <- lines <$> readFile "day3.txt"
  let grid = buildGrid datas
      numbers = buildNumbers datas
      stars = buildStars grid
  showSolution "Part1" (part1 grid numbers)
  showSolution "Part2" (part2 grid numbers stars)

-- Add numbers which are near a symbol
part1 :: M.Map (Int, Int) Char -> [Number] -> Int
part1 grid = foldl' go 0
  where
    go acc num
      |nearSymbol grid num = acc + number num
      |otherwise           = acc

-- add the product of all pairs of Number which are near a star
part2 :: M.Map (Int, Int) Char -> [Number] -> [(Int, Int)] -> Integer
part2 grid numbers = foldl' go 0
  where
    go acc star = case nearDigit grid star of
                       ps@(_:_:_) -> acc + ratio ps
                       _          -> acc

    ratio []         = error "Error: ratio: not enough parameters"
    ratio [_]        = error "Error: ratio: not enough parameters"
    ratio ps@(_:_:_) =
      case nub (map (findNumber numbers)  ps) of
        [n1, n2] -> fromIntegral (number n1 * number n2)
        _        -> 0
-- return the Number which is at postion p
findNumber :: [Number] -> (Int, Int) -> Number
findNumber numbers p = fromMaybe errorFind (find isAtPos numbers)
  where
    errorFind = error ("Error: findNumber: can't find number at: " <> show p)
    isAtPos num = line num == snd p
                  && xstart num <= fst p
                  && xend num >= fst p

-- a list of relative coordinates for neigbours.
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

-- Collect all coordinates wich are digits near a star
nearDigit :: M.Map (Int, Int) Char -> (Int, Int) ->  [(Int,Int)]
nearDigit grid star = foldr go [] positions
  where
    positions = addPair <$> [star] <*> neighbours

    go pos acc = case grid !? pos of
                   Nothing -> acc
                   (Just c)
                     | isDigit c -> pos : acc
                     | otherwise -> acc


-- return true if the Number is near a symbol.
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

-- build the grid as a Map of all coordinates
buildGrid :: [String] -> M.Map (Int, Int) Char
buildGrid ls = M.fromList (zip inds s)
  where
    s = concat ls
    ymax = length ls
    xmax = length (head ls)
    inds = [(x,y) | y <- [1..ymax], x <- [1..xmax]]

-- build a list of all Number in the input datas
-- ls is a list of input file's lines
-- We can use Map.foldrWithKey but, we need to exchange the coordinate
-- in each pair to be sure we are reading by line. It is also not clear
-- when changing of line can be manage.
buildNumbers :: [String] -> [Number]
buildNumbers ls = concat (zipWith  buildNumbers' [1..] ls)

-- build the list of all Number in a line
-- row = line number
buildNumbers' :: Int -> String -> [Number]
buildNumbers' row aline = finalize (foldr go ([],[]) (zip [1..] aline))
  where
    finalize (numbers, digits)
      | null digits = numbers
      | otherwise   = readNumber digits : numbers

    go :: (Int, Char) -> ([Number], [Number]) -> ([Number], [Number])
    go (column, c) (numbers,digits)
      | isDigit c = (numbers, Number (digitToInt c) row column column : digits)
      | null digits = (numbers, [])
      | otherwise = (readNumber digits : numbers, [])

-- when we have isolated consecutive digit in a line
-- finally caculate the Number, they constitute
readNumber :: [Number] -> Number
readNumber [] = error "Error: readNumber: empty list!"
readNumber (s:ss) = foldl' go s ss
  where go (Number n0 y0 xs0 _) (Number n1 y1 xs1 _)
          | y0 /= y1 = error "Error: readNumber: line numbers don't match"
          |otherwise = Number (n0 * 10 + n1) y0 xs0 xs1

-- build a list of coordinates of all star in the grid
buildStars :: M.Map (Int, Int) Char -> [(Int, Int)]
buildStars = M.keys . M.filter (== '*')

showSolution :: Show a => String -> a -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)
