# TDD Kata: Poker Hand

Language: Scala

## Description

This kata consists in comparing a pair of poker hands and indicate which, if either, has is the winning hand and why.

### Poker Rules

A poker deck contains 52 cards, each card has: 
* A suit which is one of Clubs (C), Diamonds (D), Hearts (H) or Spades (S).
* A value which is one of 2, 3, 4, 5, 6, 7, 8, 9, 10 (T), Jack (J), Queen (Q), King (K) or Ace (A).
 
A poker hand consists of 5 cards dealt from the deck.
Poker hands are ranked by the following partial order from lowest to highest:

* **High Card**: The order of the cards is determined by their value, being 2 the lowest and Ace the highest, suits are
  not included for ordering purposes. Ranked by the value of the highest card. If the highest cards have the same value,
  the hands are ranked by the next highest, and so on.
* **Pair**: 2 cards of the same value. Ranked by the value of the cards forming the pair.
* **Two Pairs**: 2 different pairs. Ranked by the value of their highest pair, then ranked by the value of their other
  pair, then by the value of the remaining card. 
* **Three of a Kind**: 3 cards of the same value. Ranked by the value of the 3 cards.
* **Straight**: 5 cards with consecutive values. Ranked by their highest card.
* **Flush**: 5 cards of the same suit. Ranked by their highest card.
* **Full House**: Three of a Kind + Pair. Ranked by the value of the 3 cards.
* **Four of a kind**: 4 cards with the same value. Ranked by the value of the 4 cards. 
* **Straight flush**: 5 cards of the same suit with consecutive values. Ranked by the highest card.

Examples:

| Bob (Player 1) | John (Player 2) | Output |
| -------------- | --------------- | -------------------------------------- |
| 2H 3D 5S 9C KD | 2C 3H 4S 8C AH  | Bob wins. - with high card: Ace | 
| 2H 4S 4C 2D 4H | 2S 8S AS QS 3S  | John wins. - with full house: 4 over 2 | 
| 2H 3D 5S 9C KD | 2C 3H 4S 8C KH  | John wins. - with high card: king | 
| 2H 3D 5S 9C KD | 2D 3H 5C 9S KH  | Tie |