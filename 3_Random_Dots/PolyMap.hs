--PolyMap
import Graphics.Gloss
import System.Random 
-- make sure to add random to the .cabal file build-depends section

-- cahnge the seed to get a different set of random points
randomSeed = 54321

main = do
	display (InWindow "PolyMap" (500, 500) (10, 10)) white dots

-- dots is a list circles, the first is a static oragne circle and
-- the rest are generated from our functions

dots = Pictures (
  Color myColor (Circle 10) :
  intsToDots (map toFloat (randomInts randomSeed)))

-- takes two floats and return a circle picture
mydot :: Float -> Float -> Picture
mydot x y = translate x y (Circle 1.5)

-- takes a list of floats and turns it into a list of circles
-- consumes two floats at a time 
intsToDots :: [Float] -> [Picture]
intsToDots [] = []
intsToDots (x:y:t) = (mydot x y) : intsToDots t

-- generate a list of 50 random integers
-- this gives us 25 dots (2 ints per [x, y] pair)
randomInts :: Int -> [Int]
randomInts seed = take 50 . randomRs (-250, 250) . mkStdGen $ seed

toFloat :: Int -> Float
toFloat i = (fromIntegral i)

-- using 8bit values RGBA
myColor = makeColor8 212 87 39 255 -- Orange
