-- Build me with: cabal build TP2.hs
-- Execute me with: cabal run -v0 TP2.hs
-- Load me in the REPL with: cabal repl TP2.hs, then use :r to reload the code upon changing

{- HLINT ignore -}

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "TP2 is running"

  -- let card = mkCard "knight"
  -- putStrLn (show card)
  -- let newCard = takeDamage 1 card
  -- putStrLn (show newCard)

  let deck = mkDeck (Just Knight) Nothing Nothing
  putStrLn (show deck)

  -- let board = mkBoard
  -- putStrLn (show board)


-- If you don't like this TP, you can do TP3 instead.

-- Implement a card game a la Magic the Gathering. Each player
-- has 3 spots available to play cards like this:
--
--       Player 1
--
--  card1   card2   card3
--
--
--  carda   cardb   cardc
--
--       Player2
--
-- Each player starts with a deck of cards. When it is the turn of a
-- player, he can take cards from his hand and put them on the board.
-- After that, the turn resolves: each card of the player attacks the
-- card in front of it. If a player has no card in front of the attacking
-- card, then the attack contributes to the player's score. Each card
-- has two stats: its hitpoints and its attack. When a card attacks, it
-- deals the corresponding hitpoint to the opponent card.
-- Consider the diagram below:
--
--      Player 1
--
-- knight   empty   soldier
--
-- empty    soldier soldier
--
--      Player 2
--
-- with the knight having 2 hitpoints and 2 attacks, and the soldier having 1 hitpoint
-- and 1 attack. In this scenario, when player 1 attacks, the left knight
-- contributes two to the score while the right soldier kills its opponent (there
-- are only two cards: knight and soldier)
--
-- Exercise:
--
-- 1. Define a type for cards
--    - Define a Show instance for this type
-- 2. Define a type for the part of a player. Make it implement Show.
-- 3. Define a type for the whole board: the two players parts. Make it implement Show.
-- 4. Write a function making a player play
-- 5. Write a function playing an entire game
--    Display the list of boards while the game runs.
--
-- If you want to have randomness, use the 'random' function here:
-- https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html#v:random
-- Use mkStdGen to obtain a value that satisfies the constraint "RandomGen g":
-- https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html#t:StdGen
--
-- Use https://hoogle.haskell.org/ to find the functions you need



data CardKind = Knight | Soldier


instance Show CardKind where
  show Knight = "Knight            "
  show Soldier = "Soldier          "


data Card = MkCard {
  name :: CardKind,
  healthPoints :: Int,
  attackPoints :: Int
}


showCardName :: Card -> String
showCardName card = show (name card)


showCardInfo :: Card -> String
showCardInfo card = "(HP: " ++ show (healthPoints card) ++ ", ATK: " ++ show (attackPoints card) ++ ")   "


mkCard :: CardKind -> Card
mkCard Knight = MkCard { name = Knight, healthPoints = 2, attackPoints = 2}
mkCard Soldier = MkCard { name = Soldier, healthPoints = 1, attackPoints = 1}


mkMaybeCard :: Maybe CardKind -> Maybe Card
mkMaybeCard (ck :: Maybe CardKind) = fmap mkCard ck 


takeDamage :: Int -> Card -> Card
takeDamage n card = card { healthPoints = healthPoints card - n }


attack :: Card -> Card -> Maybe Card
attack card1 card2 = result where
  result
    | hp > 0 = Just damagedCard
    | otherwise = Nothing
    where
      damagedCard = takeDamage (attackPoints card1) card2
      hp = healthPoints damagedCard


data Deck = MkDeck {
  card1 :: Maybe Card,
  card2 :: Maybe Card,
  card3 :: Maybe Card
}


instance Show Deck where
  show deck = "card 1            card 2            card 3\n" ++ showMaybeCardName (card1 deck) ++ showMaybeCardName (card2 deck) ++ showMaybeCardName (card3 deck) ++ "\n"  ++ showMaybeCardInfo (card1 deck) ++ showMaybeCardInfo (card2 deck) ++ showMaybeCardInfo (card3 deck)
    where
      showMaybeCardName Nothing = "Empty             "
      showMaybeCardName (Just card) = showCardName card
      showMaybeCardInfo Nothing = "                  "
      showMaybeCardInfo (Just card) = showCardInfo card


mkDeck :: (Maybe CardKind) -> (Maybe CardKind) -> (Maybe CardKind) -> Deck
mkDeck ck1 ck2 ck3 = MkDeck (mkMaybeCard ck1) (mkMaybeCard ck2) (mkMaybeCard ck3)


data Board = MkBoard {
  nPoints_p1 :: Int,
  deck_p1 :: Deck,
  nPoints_p2 :: Int,
  deck_p2 :: Deck
}


mkBoard :: Board
mkBoard = MkBoard { nPoints_p1 = 0, deck_p1 = mkDeck Nothing Nothing Nothing , nPoints_p2 = 0, deck_p2 = mkDeck Nothing Nothing Nothing }


instance Show Board where
  show board = "========\nplayer 1\npoints: " ++ show (nPoints_p1 board) ++ "\n" ++ show (deck_p1 board) ++ "\n\nplayer 2\npoints: "++ show (nPoints_p2 board) ++ "\n" ++ show (deck_p2 board)


getUserMoves :: Deck -> IO Deck
getUserMoves deck = do
  action1 <- case deck.card1 of
    Nothing -> do
      putStrLn "Enter your move for the card number 1, you can either do nothing ('nothing'), place a knight ('knight'), or place a soldier ('soldier')"
      getLine
    _ -> do
      return "nothing"
  action2 <- case deck.card2 of
    Nothing -> do
      putStrLn "Enter your move for the card number 2, you can either do nothing ('nothing'), place a knight ('knight'), or place a soldier ('soldier')"
      getLine
    _ -> do
      return "nothing"
  action3 <- case deck.card3 of
    Nothing -> do
      putStrLn "Enter your move for the card number 3, you can either do nothing ('nothing'), place a knight ('knight'), or place a soldier ('soldier')"
      getLine
    _ -> do
      return "nothing"
  return deck {
    card1 = if action1 == "nothing" then deck.card1 else if action1 == "knight" then Just (mkCard Knight)    else if action1 == "soldier" then Just (mkCard Soldier) else undefined,
    card2 = if action2 == "nothing" then deck.card2 else if action2 == "knight" then Just (mkCard Knight)    else if action2 == "soldier" then Just (mkCard Soldier) else undefined,
    card3 = if action3 == "nothing" then deck.card3 else if action3 == "knight" then Just (mkCard Knight)    else if action3 == "soldier" then Just (mkCard Soldier) else undefined
  }


attack :: Board -> Int -> Board
attack _ player | (player < 1) || (player > 2) = undefined
attack board 1 = undefined
attack board 2 = undefined


oneTurn :: Board -> IO Board
oneTurn board player = do
  deck1 <- getUserMoves board.deck1
  undefined
