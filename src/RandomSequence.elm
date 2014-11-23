module RandomSequence where

import Random
import Text (asText)
import Graphics.Element (Element)

-- A Seed is used to generate a reproducable sequence of pseudo random values.
-- Changing this value will result in a different but repeatable results.
seed : Random.Seed
seed = 
    Random.initialSeed 42
    --Random.initialSeed 43
    --Random.initialSeed 44
    --Random.initialSeed 45


randomSequence : List Int
randomSequence =
    -- Generate a random value between 0 and 10
    -- This returns a pair containing the generated value
    -- and a new seed that can be used to generate a different
    -- value
    let (n0, seed1) = Random.int 0 10 seed
        (n1, seed2) = Random.int 0 10 seed1
    -- Threading the new seed allows you to produce a sequence
    -- of pseudo random values
        (n2, seed3) = Random.int 0 10 seed2
        (n3, seed4) = Random.int 0 10 seed3
    in [n0, n1, n2, n3]

main : Element
main = asText randomSequence