-- https://www.codewars.com/kata/525c65e51bf619685c000059
module Baker where

type Ingredient = String
type Amount     = Int
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage = maybe 0 id $ minimum $ zipWith (<$>) quantities names
  where names = flip lookup storage . fst <$> recipe
        quantities = flip div . snd <$> recipe
