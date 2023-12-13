#! /usr/bin/ruby

# frozen_string_literal: true

require 'strscan'

# a Card is represent as the card number and the score of the card.
class Card
  def initialize(str)
    @card, wins, nums = parse_card(str)
    @score = wins.reduce(0) do |acc, n|
      if nums.include?(n)
        (acc + 1)
      else
        acc
      end
    end
  end
  attr_reader :card, :score

  def parse_card(str)
    s = StringScanner.new(str)
    s.skip(/^Card +/)
    card = s.scan(/\d+/).to_i
    s.skip(/: +/)
    wins = s.scan(/(\d+ +)+/).split(/ +/).collect(&:to_i)
    s.skip(/\| +/)
    nums = s.scan(/(\d+ *)+$/).split(/ +/).collect(&:to_i)
    [card, wins, nums]
  end
  private :parse_card
end

def part1(cards)
  cards.reduce(0) do |acc, card|
    n = card.score
    if n.zero?
      acc
    else
      acc + 2**(n - 1)
    end
  end
end

def count_cards(allcards, subcards)
  subcards.reduce(0) do |acc, card|
    n = card.card
    sc = card.score
    1 + acc + count_cards(allcards, allcards[n, sc])
  end
end

def part2_naive(cards)
  count_cards(cards, cards)
end

def part2_opt(cards)
  memo = cards.reverse.reduce([]) do |acc, c|
    n = c.score
    s = acc.take(n).sum + 1
    acc.unshift(s)
  end
  memo.sum
end
cards = File.readlines('day4.txt', chomp: true).collect { |s| Card.new(s) }

puts "Part1: #{part1(cards)}"
puts "Part2: #{part2_opt(cards)}"
