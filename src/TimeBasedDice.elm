module TimeBasedDice where

import Random
import Basics (..)
import Text (asText)
import Graphics.Element (..)
import Color
import List
import Signal (Signal, foldp, (<~), (~), constant, sampleOn, keepWhen)
import Mouse

import Time (Time)
import Time

import Array (Array)
import Array

import Maybe ((?), Maybe(Just, Nothing))

-----------
-- Model --
-----------

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
initialState : Random.Seed -> State
initialState s = { rolls = Array.repeat 11 0
                 , seed = s
                 , lastRoll = (1, 2)
                 }

-- This bit is sketchy... we really need a way for the constant signal to be fired once after
-- the page loads so that the handleInput function fires right after the page loads
-- This whole bit here does that... but it is really innefficient.
-- I originally wrote this:

{- 
startTime : Signal Time
startTime = (\ (t,_) -> t) <~ sampleOn (Time.fpsWhen 60 okay) (Time.timestamp <| constant ())

timesFired : Signal Int
timesFired = foldp (+) 0 (sampleOn startTime (constant 1))
 
okayToSample : Int -> Bool
okayToSample fired = fired < 2

okay : Signal Bool
okay = okayToSample <~ timesFired

seed : Signal (Maybe Random.Seed)
seed = (\ t -> Just << Random.initialSeed <| round t) <~ startTime
-}

-- Although it type checks, this causes a run time error. Perhaps because the signals are recursive?

start = (\ (t,_) -> t) <~ (Time.timestamp <| constant ())

startTime : Signal (Maybe Time)
startTime = (\ (t,_) -> Just t) <~ sampleOn (Time.fps 10) (Time.timestamp <| constant ())

okayToSample : Int -> Bool
okayToSample fired = fired < 2

okay : Signal Bool
okay = okayToSample <~ (foldp (+) 0 (sampleOn startTime (constant 1)))

seed : Signal (Maybe Random.Seed)
seed = (\ t -> 
            case t of
              Nothing -> Nothing
              Just t -> Just << Random.initialSeed <| round t) <~ keepWhen okay Nothing startTime


------------
-- Update --
------------

-- Get two values between 1 and 6, sum them, and update the state
roll : State -> State
roll state =
    let (first_roll, seed') = Random.int 1 6 state.seed
        (second_roll, seed'') = Random.int 1 6 seed'
        ix = (first_roll + second_roll) - 2
        curr = (Array.get ix state.rolls) ? 0
        rolls' = Array.set ix (curr + 1) state.rolls
    in { rolls = rolls', seed = seed'', lastRoll = (first_roll, second_roll) }


-- Initialize the seed if necessary, otherwise
-- Each time someone clicks, roll the dice.
handleInput : (Maybe Random.Seed, ()) -> Maybe State -> Maybe State
handleInput (maybeSeed, _) state = 
    case maybeSeed of
      Nothing -> Nothing
      Just seed -> 
          case state of
            Nothing -> Just (initialState seed)
            Just s -> Just (roll s)

-- Basic wiring
main : Signal Element
main = display <~ (foldp handleInput Nothing ((,) <~ seed ~ Mouse.clicks))

----------
-- View --
----------

-- Displays the last rolled dice above a chart of previous rolls
display : Maybe State -> Element
display state =
    case state of
      Nothing -> asText "Loading..."
      Just s ->
    let (die0, die1) = s.lastRoll
        dice = container 300 100 midTop <| flow right [dieImage die0, dieImage die1]
        graph = container 300 200 midTop <| chart s.rolls
    in flow down [ spacer 1 20, dice, graph ]


-- Selects the correct die graphic to display
dieImage : Int -> Element
dieImage n = image 100 100 ("assets/" ++ (toString n) ++ ".png")

-- Charts the rolls or says No Data if there is no data yet
chart : Array Int -> Element
chart rolls = 
    -- Finds the maximum value of all the rolls
    let maxRoll = Array.foldr max 0 rolls
    in if | maxRoll == 0 -> asText "No data, click to roll."
          | otherwise ->
              let getRollBar n = bar n ((Array.get n rolls) ? 0) maxRoll
              in  flow right (List.map getRollBar [0..10])

-- Draw a bar up to 100px tall based on how many times we've seen it
-- and label it
bar : Int -> Int -> Int -> Element
bar n rolls max_rolls =
    let h = round <| ((toFloat rolls) / (toFloat max_rolls)) * 100
        theBar = height h empty |> width 25 |> color Color.blue
        label = container 25 25 middle <| asText (n + 2)
        total = container 25 25 middle <| asText rolls
    in flow right [ flow down [label, theBar, total]
                  , spacer 1 1
                  ]

