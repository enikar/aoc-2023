-- Solution of AoC 2023, 2nd day

module Main where

import Data.Char (isDigit)
import Data.List (foldl')
import Control.Applicative ((<|>))
import Control.Monad (void)
import Text.ParserCombinators.ReadP

parse :: ReadP a -> ReadS a
parse = readP_to_S


main :: IO ()
main = do
  games <- readGames . lines <$> readFile "day2.txt"
  showSolution "Part1: " (part1 games)
  showSolution "Part2: " (part2 games)

data Subset = Subset {red :: Int
                     ,green :: Int
                     ,blue :: Int
                     } deriving Show

data Game = Game {game :: Int
                 ,subsets :: [Subset]
                 } deriving Show

part1 :: [Game] -> Int
part1 = foldl' go 0
  where
    go acc (Game g subs) = if all acceptable subs
                           then acc + g
                           else acc
    acceptable (Subset r g b) = r <= 12 && g <= 13 && b <= 14

part2 :: [Game] -> Int
part2 games = foldl' (+) 0 (map power games)
  where
    power (Game _ subs) = red sub * green sub * blue sub
      where sub = foldl' power' (Subset 0 0 0) subs
            power' (Subset r1 g1 b1) (Subset r2 g2 b2) = Subset (max r1 r2) (max g1 g2) (max b1 b2)

readGames :: [String] -> [Game]
readGames = map parseGame

parseGame :: String -> Game
parseGame s = (fst . head) (parse readGame s)

readGame :: ReadP Game
readGame = do
  void (string "Game ")
  n <- number 1 <|> number 2 <|> number 3
  void (string ": ")
  sets <- sepBy1 readSubset (string ";")
  eof
  pure (Game n sets)

readSubset :: ReadP Subset
readSubset = do
  skipSpaces
  cubes <-  sepBy1 readCube (string ",")
  let sub = foldl' makeSub (Subset {red = 0, green = 0, blue = 0}) cubes
      makeSub acc cube = case cube of
        (n, "red") -> acc {red = n}
        (n, "green") -> acc {green = n}
        (n, "blue") -> acc {blue = n}
        (_, c) -> error ("Error: readSubset: no such color: " <> c)
  pure sub

readCube :: ReadP (Int, String)
readCube = do
  skipSpaces
  n <- number 1 <|> number 2
  skipSpaces
  c <- string "red" <|> string "green" <|> string "blue"
  pure (n, c)


digit :: ReadP Char
digit = satisfy isDigit

number :: Int -> ReadP Int
number n = fmap read (count n digit)

showSolution :: Show a => String -> a -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)
