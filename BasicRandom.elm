module BasicRandom where

import Random
import Text (asText)
import Graphics.Element (Element)

-- A Seed is used to create a reproducable sequence of pseudo random values.
-- Changing this value will result in a different but repeatable sequence
seed : Random.Seed
seed = 
    Random.initialSeed 42
    -- Random.initialSeed 43
    -- Random.initialSeed 44
    -- Random.initialSeed 45

-- Generate a random Int between 0 and 10
randomInteger : Int
randomInteger = 
    let (n, _) = Random.int 0 10 seed
    in n

-- Generate a random Float between 0 and 100
randomFloat : Float
randomFloat =
    let (f, _) = Random.float 0 100 seed
    in f

-- Generate a list of 20 random Ints between 0 an 100
randomList : (List Int)
randomList = 
    let (ls, _) = Random.list 20 (Random.int 0 100) seed
    in ls

main : Element
main = 
    asText randomInteger
    -- asText randomFloat
    -- asText randomList
