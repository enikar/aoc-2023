-- Solution of AoC 2023, 4th day
module Main where

import Data.Char (isDigit)
import Data.List (foldl')
import Control.Monad (void)
import Text.ParserCombinators.ReadP (string
                                    ,char
                                    ,skipSpaces
                                    ,sepBy1
                                    ,endBy1
                                    ,munch1
                                    ,eof
                                    ,readP_to_S
                                    ,ReadP)

data Card = Card {card :: Int
                 ,score :: Int
                 } deriving Show

main :: IO ()
main = do
  cards <- parseCards <$> readFile "day4.txt"
  showSolution "Part1" (part1 cards)

showSolution :: Show a => String -> a -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)

part1 :: [Card] -> Int
part1 = foldl' go 0
  where
    go acc card = acc + countPoints card

powerOfTwo :: Int -> Int
powerOfTwo n = 2 ^ n

countPoints :: Card -> Int
countPoints card
  | n == 0 = 0
  | otherwise = powerOfTwo (n-1)
    where
      n = score card

parseCards :: String -> [Card]
parseCards = fst . head . parse readCards

readCards :: ReadP [Card]
readCards = do
  cards <- endBy1 readCard (char '\n')
  eof
  pure cards

readCard :: ReadP Card
readCard = do
  void (string "Card")
  skipSpaces
  n <- number
  void (char  ':')
  skipSpaces
  wins <- sepBy1 number spaces
  skipSpaces
  void (char '|')
  skipSpaces
  nums <- sepBy1 number spaces
  let cardScore = foldl' go 0 wins
      go acc k
        | k `elem` nums = acc + 1
        | otherwise     = acc
  pure (Card {card=n, score=cardScore})

number :: ReadP Int
number = read <$> munch1 isDigit

spaces :: ReadP String
spaces = munch1 (== ' ')

parse :: ReadP a -> ReadS a
parse = readP_to_S
