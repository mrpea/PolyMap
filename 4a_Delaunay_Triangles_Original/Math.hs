--Math.hs
module Math  where


data Pt = Pt Float Float deriving (Show, Eq)
data Tri = Tri Pt Pt Pt deriving (Show, Eq)
data Cir = Cir Pt Float deriving (Show, Eq)
{--


instance Show Pt where
   show (Pt x y) = show x
--}

prtPt :: Pt -> String
prtPt (Pt x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

prtPtLst :: [Pt] -> String
prtPtLst [] = ""
prtPtLst (h:t) = (prtPt h) ++ " " ++ (prtPtLst t)

pX :: Pt -> Float
pX (Pt x y) = x

pY :: Pt -> Float
pY (Pt x y) = y


lineSlope :: Pt -> Pt -> Float
lineSlope (Pt x1 y1) (Pt x2 y2) = 
    let mY = slopeHelper y2 y1
        mX = slopeHelper x2 x1
        m = mY / mX
	in if (m == 0 || isNaN m) then 0.0001 else m


slopeHelper :: Float -> Float -> Float
slopeHelper a b
    | a == b = 0.0001
    | otherwise = a - b


centerFromPts :: Pt -> Pt -> Pt -> Pt
centerFromPts (Pt x1 y1) (Pt x2 y2) (Pt x3 y3) =
   let mA = (lineSlope (Pt x1 y1) (Pt x2 y2))
       mB = (lineSlope (Pt x2 y2) (Pt x3 y3)) 
       newX = ((mA * mB * (y1 - y3)) + (mB * (x1 + x2)) - (mA * (x2 + x3))) / (2 * (mB - mA))
       newY = ((-1) * (1 / mA) * (newX - ((x1 + x2)/2))) + ((y1 + y2)/2)
   in (Pt newX newY)

distance :: Pt -> Pt -> Float
distance (Pt x1 y1) (Pt x2 y2) = sqrt (((x2 - x1)^2) + ((y2 - y1)^2))

ptInCir :: Pt -> Cir -> Bool
ptInCir pPt@(Pt pX pY) (Cir pCircle@(Pt cX cY) radius)
    | pX < cX - radius = False
    | pX > cX + radius = False
    | pY < cY - radius = False
    | pY > cY + radius = False
    | otherwise = (distance pPt pCircle) < radius

ptInCirRev :: Cir -> Pt -> Bool
ptInCirRev c p = ptInCir p c

--Given a set of triangles and a set of points find the circumcirles that
--do not have a point in them
delaunayTri :: [Tri] -> [Pt] -> [Tri]
delaunayTri [] [] = []
delaunayTri _ [] = []   
delaunayTri [] _ = [] 
--delaunayTri (t:[]) pts = (checkTri t pts)
delaunayTri (t:ts) pts = (checkTri2 t pts) ++ delaunayTri ts pts

-- Given a triangle and a set of points check if the 
-- points exist in the circumcircle of the triangle	
-- If the points are in the circle then return nothing, else the
-- original triangle
checkTri :: Tri -> [Pt] ->[Tri]
checkTri t [] = [t]
checkTri t@(Tri p1 p2 p3) pts =
  let checkPts = (removeItem p3 (removeItem p2 (removeItem p1 pts)))
      badPts = (filter (ptInCirRev (circumscribeTri t)) checkPts )
  in if (
  		 -- disqualify triangle if there are points in the circle	
  		 ((length badPts) /= 0) 
  		 -- disqualify triangle if all points have same x or same y
  	       || ((pX p1) == (pX p2) && (pX p2) == (pX p3)) 
  	       || ((pY p1) == (pY p2) && (pY p2) == (pY p3))
         -- disqualify triangle if all points on same line 
  	       || lineSlope p1 p2 == lineSlope p2 p3 ) 
     then [] else [t]

checkTri2 :: Tri -> [Pt] ->[Tri]
checkTri2 t [] = [t]
checkTri2 t@(Tri p1 p2 p3) pts
    | ((pX p1) == (pX p2) && (pX p2) == (pX p3)) = []
    | ((pY p1) == (pY p2) && (pY p2) == (pY p3)) = []
    | lineSlope p1 p2 == lineSlope p2 p3 = [] 
    | (length badPts) /= 0 = []
    | otherwise = [t]
    where checkPts = (removeItem p3 (removeItem p2 (removeItem p1 pts)))
          badPts = (filter (ptInCirRev (circumscribeTri t)) checkPts )

--given a triangle find the circle that circumscribes the points
circumscribeTri :: Tri -> Cir
circumscribeTri (Tri p1 p2 p3) =
	let center = centerFromPts p1 p2 p3
	    radius = distance p2 center   
	in Cir center radius

-- helper function
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys