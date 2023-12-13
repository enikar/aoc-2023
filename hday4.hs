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
  showSolution "Part2" (part2_opt cards)

showSolution :: Show a => String -> a -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)

part1 :: [Card] -> Int
part1 = foldl' go 0
  where
    go acc card
      | n == 0 = acc
      | otherwise = acc + powerOfTwo (n-1)
        where
          n = score card

part2_naive :: [Card] -> Int
part2_naive cards = countCards cards
    where
      countCards = foldl' go 0
      go acc c = 1 + acc + countCards subCards
        where
          n = card c
          subCards = take (score c) (drop n cards)

-- part2_naive is very slow because we evaluate many times
-- the same things. We are building the number of copies of each card
-- incrementally.
-- But if we build a list of number of copies from the ends of the cards
-- list, we can memoize these number without the need to compute them
-- again. Thus we'll use a foldr that builds a list of numbers copies
-- for each card. At each step the number we add one (the original card)
-- to the number of copies the current card wins. And push this number to
-- the memoizing list.
part2_opt :: [Card] -> Int
part2_opt = sum . foldr go []
  where
    go c acc = 1 + sum (take (score c) acc) : acc

powerOfTwo :: Int -> Int
powerOfTwo n = 2 ^ n

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
