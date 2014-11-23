module Dice where

import Random
import Basics (..)
import Text (asText)
import Graphics.Element (..)
import Color
import List
import Signal (Signal, foldp, (<~))
import Mouse
import Time (fps)
import Debug

import Array (Array)
import Array

{-
  rolls - The different possible rolls (offset by 2) and the number of times
          it has been rolled

  seed - The seed to use to generate pseudo random numbers

  last_roll - The last roll that occurred.

-}
type alias State = { rolls : Array Int
                   , seed  : Random.Seed 
                   , lastRoll : (Int, Int)
                   }

-- The initial state contains the zero for each possible roll
-- Starts with a seed of 42 (changing this will change the outcome of the sequence)
initialState : State
initialState = { rolls = Array.repeat 11 0
               , seed = Random.initialSeed 42
               , lastRoll = (1, 2)
               }

-- Get two values between 1 and 6, sum them, and update the state
roll : State -> State
roll state =
    let (first_roll, seed') = Random.int 1 6 state.seed
        (second_roll, seed'') = Random.int 1 6 seed'
        ix = (first_roll + second_roll) - 2
        curr = Array.getOrElse 0 ix state.rolls
        rolls' = Array.set ix (curr + 1) state.rolls
    in { rolls = rolls', seed = seed'', lastRoll = (first_roll, second_roll) }

-- Selects the correct die graphic to display
dieImage : Int -> Element
dieImage n = image 100 100 ("assets/" ++ (toString n) ++ ".png")

display : State -> Element
display { rolls, lastRoll } =
    let (die0, die1) = lastRoll
        dice = container 300 100 midTop <| flow right [dieImage die0, dieImage die1]
        graph = container 300 200 midTop <| chart rolls
    in flow down [ spacer 1 20, dice, graph ]

-- Charts the rolls or says No Data if there is no data yet
chart : Array Int -> Element
chart rolls = 
    let maxRoll = Array.foldr max 0 rolls
    in if | maxRoll == 0 -> asText "No data, click to roll."
          | otherwise ->
              let getRollBar n = bar n (Array.getOrElse 0 n rolls) maxRoll
              in  flow right (List.map getRollBar [0..10])

-- Draw a bar and label it
bar : Int -> Int -> Int -> Element
bar n rolls max_rolls =
    let h = round <| ((toFloat rolls) / (toFloat max_rolls)) * 100
        theBar = height h empty |> width 25 |> color Color.blue
        label = container 25 25 middle <| asText (n + 2)
        total = container 25 25 middle <| asText rolls
    in flow right [ flow down [label, theBar, total]
                  , spacer 1 1
                  ]

-- Each time someone clicks, roll the dice.
handleClick : Float -> State -> State
handleClick _ state = roll state

-- Basic wiring
main : Signal Element
main = display <~ (foldp handleClick initialState (fps 60))