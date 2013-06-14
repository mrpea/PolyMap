--PolyMap
import Graphics.Gloss

main = do
	display (InWindow "PolyMap" (500, 500) (10, 10)) white (Circle 120)

{-|

    Breaking it down:

	display (InWindow 
			 "PolyMap"    -- Window title
			 (500, 500)   -- Window size
			 (10, 10))    -- Window location in your OS
			 white        -- Background color
			 (Circle 120) -- Content: Must be Gloss.Data.Picture data type

-}						