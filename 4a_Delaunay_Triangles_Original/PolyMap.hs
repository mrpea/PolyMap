--PolyMap
import Graphics.Gloss
import System.Random 
-- make sure to add random to the .cabal file build-depends section
import Text.Show.Functions

import Math

-- change the seed to get a different set of random points
randomSeed = 88321
numDots = 100
windowHeight = 1000
windowWidth = 1000
margin = 500

maxR = 300
minR = (-300)

main = do
	--(prtPtLst tOrgLst)
	print dots
	--display (InWindow "PolyMap" (windowWidth, windowHeight) (10, 10)) white dots
    
-- dots is a list circles, the first is a static oragne circle and
-- the rest are generated from our functions

floatList = (map toFloat (randomInts randomSeed))
ptList = (floatToPt floatList)

allTris = ptToTri (combinations 3 ptList)
goodTris = delaunayTri allTris ptList
goodCircles = cirToCircleLst (map circumscribeTri goodTris)

testC = circumscribeTri (goodTris !! 40)

testCir = (take 10 (drop 40 (reverse (map (Color ltGray) goodCircles))))
testTri = (take 10 (drop 40 (reverse (map triToLine goodTris) )))
testT = circumscribeTri (Tri (Pt 5 10) (Pt 6 12) (Pt 3 4))
testT2 = circumscribeTri (Tri (Pt 5 10) (Pt 5.1 12) (Pt 3 4))

testT3 = circumscribeTri (Tri (Pt 232 (-463)) (Pt 232 (-91)) (Pt 178 (-32)))


dots = Pictures (	
	-- testCir ++ testTri
	(map (Color ltGray) goodCircles) -- circles    
    ++ (map triToLine goodTris) --triangles
    ++ (map (Color myColor) (intsToDots floatList)) -- random dots
  )

-- takes two floats and returns a circle picture
mydot :: Float -> Float -> Picture
mydot x y = translate x y (circleSolid 2)

-- takes a list of floats and turns it into a list of circles
-- consumes two floats at a time 
intsToDots :: [Float] -> [Picture]
intsToDots [] = []
intsToDots (x:y:t) = (mydot x y) : intsToDots t

-- generate a list of 50 random integers
-- this gives us 25 dots (2 ints per [x, y] pair)
randomInts :: Int -> [Int]
randomInts seed = take (numDots * 2) . randomRs (minR, maxR) . mkStdGen $ seed

toFloat :: Int -> Float
toFloat i = (fromIntegral i)

-- takes a list of floats and creates a list of points
-- each two successive floats is the x,y coords for the point
floatToPt :: [Float] -> [Pt]
floatToPt [] = []
floatToPt (x:y:t) = (Pt x y) : floatToPt t

ptToCircle :: Pt -> Pt-> Pt -> Picture
ptToCircle p1 p2 p3 = 
	let center = centerFromPts p1 p2 p3
	    radius = distance p2 center
	    x = (pX center)
	    y = (pY center)
	in translate x y (Circle radius)

--trisToCircles :: [Tri] -> [Circle]	

ptToTri :: [[Pt]] -> [Tri]
ptToTri [] = []
ptToTri (pts:t) = Tri (pts !! 0) (pts !! 1) (pts !! 2) : ptToTri t

triToLine :: Tri -> Picture
triToLine (Tri (Pt x1 y1) (Pt x2 y2) (Pt x3 y3)) = lineLoop [(x1,y1), (x2,y2), (x3,y3)]

cirToCircleLst :: [Cir] -> [Picture]
cirToCircleLst [] = []
cirToCircleLst (c:t) = (cirToCircle c) : cirToCircleLst t

cirToCircle :: Cir -> Picture
cirToCircle (Cir p r) = translate (pX p) (pY p) (Circle r)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs@(y:ys)
  | n < 0     = []
  | otherwise = case drop (n-1) xs of
                  [ ] -> []
                  [_] -> [xs]
                  _   -> [y:c | c <- combinations (n-1) ys]
                            ++ combinations n ys

-- using 8bit values RGBA
myColor = makeColor8 212 87 39 255 -- Orange
ltGray = makeColor8 230 230 230 255 -- Light Gray
myBlue = makeColor8 10 10 255 255
