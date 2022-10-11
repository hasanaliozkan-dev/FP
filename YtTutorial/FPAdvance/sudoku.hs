
type  Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

--type Grid = [[Char]]
boxSize :: Int
boxSize = 3

values :: [Value]
values = ['1' .. '9']

empty :: Value -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _   = False

puzzle :: Grid
puzzle = ["2....1.38",
          "........5",
          ".7...6...",
          ".......13",
          ".981..257",
          "31....8..",
          "9..8...2.",
          ".5..69784",
          "4..25...."]


valid :: Grid -> Bool
valid g = all noDups (rows g) &&
          all noDups (cols g) &&
          all noDups (boxes g)

noDups :: Eq a => [a] -> Bool
noDups [] = True
noDups (x : xt) = not (elem x xt) && noDups xt

rows :: Matrix a -> [Row a]
rows =  id

cols :: Matrix a -> [Row a]
cols = transpose

boxes :: Matrix a -> [Row a]
boxes = unpack . map cols . pack
        where
          pack   = split . map split
          split  = chop boxSize
          unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)



type Choices = [Value]

choices :: Grid -> Matrix Choices
choices g = map (map choice) g
            where
              choice v = if empty v then values else [v]

