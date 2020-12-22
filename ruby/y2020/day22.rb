# Crab Combat

class Player
  # The deck of cards is a simple array, with the top card at deck[0].
  attr_accessor :deck

  def initialize(deck)
    @deck = deck
    @deck_stack = []
  end

  def push_deck(n)
    new_deck = @deck[0, n].dup
    @deck_stack.push(@deck)
    @deck = new_deck
  end

  def pop_deck
    @deck = @deck_stack.pop
  end

  def score
    total = 0
    deck.reverse.each_with_index { |card, i| total += card * (i+1) }
    total
  end
end


class Day22 < Day
  def part1
    winner = do_part1
    puts(winner.score)
  end

  def part1_tests
    expected = 306
    winner = do_part1()
    answer = winner.score
    if answer == expected
      puts('.')
      puts('ok')
    else
      puts('F')
      puts("error: expected #{expected}, got #{answer}")
    end
  end

  def do_part1
    player1, player2 = parse()
    while true
      card1 = player1.deck.shift
      card2 = player2.deck.shift
      winner, loser = card1 > card2 ? [player1, player2] : [player2, player1]
      winner.deck += [card1, card2].sort.reverse
      return winner if loser.deck.empty?
    end
  end

  def part2
    player1, player2 = parse()
    winner = do_part2(player1, player2)[0]
    puts winner.score
  end

  def part2_tests
    expected = 291
    player1, player2 = parse()
    winner = do_part2(player1, player2)[0]
    answer = winner.score
    if answer == expected
      puts('.')
      puts('ok')
    else
      puts('F')
      puts("error: expected #{expected}, got #{answer}")
    end
  end

  def do_part2(player1, player2, game_num=1)
    previous_rounds = {}
    round_num = 1
    while true
      # if hand has been seen before, player 1 wins
      round_start = player1.deck + player2.deck
      return [player1, player2] if previous_rounds.include?(round_start)

      card1 = player1.deck.shift
      card2 = player2.deck.shift

      # Check for empty hands
      return [player1, player2] if card2.nil?
      return [player2, player1] if card1.nil?

      if player1.deck.length >= card1 && player2.deck.length >= card2
        # play a recursive game
        player1.push_deck(card1)
        player2.push_deck(card2)
        winner, loser = do_part2(player1, player2, game_num+1)
        player1.pop_deck
        player2.pop_deck
      else
        # a normal hand
        winner, loser = card1 > card2 ? [player1, player2] : [player2, player1]
      end

      # Winning card order is determined by who the winner was, not the
      # value of the cards.
      cards = winner == player1 ? [card1, card2] : [card2, card1]
      winner.deck += cards
      return [winner, loser] if loser.deck.empty?
      round_num += 1
    end
  end

  def parse
    players = []
    deck = []
    data_lines(1).each do |line|
      if line =~ /Player (\d+)/
        unless deck.empty?
          players << Player.new(deck)
        end
        deck = []
      else
        deck << line.to_i
      end
    end
    players << Player.new(deck)
  end
end
