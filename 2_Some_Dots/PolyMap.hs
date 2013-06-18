--PolyMap
import Graphics.Gloss

main = do
	display (InWindow "PolyMap" (500, 500) (10, 10)) white dots

dots = Pictures [
          (Circle 5), -- Center
          translate 0 150 (Circle 10), -- Top
          translate 150 0 (Circle 20), -- Right
          translate 0 (-150) (Circle 30), -- Bottom
          translate (-150) 0 (Circle 40), -- Left
          
          -- Color examples
          translate (-75) (-220) (Color red (Circle 5)), -- Left
          translate (-25) (-220) (Color green (Circle 5)), 
          translate 25 (-220) (Color blue (Circle 5)), 
          translate 75 (-220) (Color myColor (ThickCircle 5 2)) -- Right, 
                                                                -- Custom Color,
                                                                -- Line Thickness
       ]

myColor = makeColor8 212 87 39 255 -- Orange
