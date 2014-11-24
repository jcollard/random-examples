module TimeBasedRandom where

import Random

import Time (Time)
import Time
import Signal (Signal, (<~))
import Signal

import Text (asText)

seed : Signal Random.Seed
seed = (\ (t, _) -> Random.initialSeed <| round t) <~ Time.timestamp (Signal.constant ())

randomList : Random.Seed -> List Int
randomList seed = 
    let (ls, _) = Random.list 20 (Random.int 0 100) seed
    in ls

main = asText <~ (randomList <~ seed)