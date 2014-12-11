module BasicRandom where

import Random
import Text (asText)
import Graphics.Element (Element)

-- A Seed is used to create a reproducable sequence of pseudo random values.
-- Changing this value will result in a different but repeatable results.
seed : Random.Seed
seed = 
    Random.initialSeed 42
    --Random.initialSeed 43
    --Random.initialSeed 44
    --Random.initialSeed 45

-- Generate a random Int between 0 and 100
-- Random.int : Int -> Int -> Generator Int
randomInteger : Int
randomInteger = 
    -- Returns a pair, the first value is the generated value
    let (n, _) = Random.generate (Random.int 0 100) seed
    in n

-- Generate a random Float between 0 and 1
-- Random.float : Float -> Float -> Generator Float
randomFloat : Float
randomFloat =
    -- Returns a pair, the first value is the generated value
    let (f, _) = Random.generate (Random.float 0 1) seed
    in f

-- Generate a list of 20 random Ints between 0 an 100
-- Random.list : Int -> Generator a -> Generator (List a)
randomList : (List Int)
randomList = 
    -- Returns a pair, the first value is a generated value
    let (ls, _) = Random.generate (Random.list 20 (Random.int 0 100)) seed
    in ls

main : Element
main = 
    asText randomInteger
    --asText randomFloat
    --asText randomList
