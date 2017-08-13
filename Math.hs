module Math where

-- why is 2d int, others double?
type Point2D = (Int, Int)
type Point3D = (Double, Double, Double)
type Vector = (Double, Double, Double)

data Ray = Ray Point3D Vector

data Object = Sphere Double Point3D
  | Plane (Double, Double, Double, Double)

type Resolution = (Int, Int)
type Dimension = (Int, Int)

(<+>) :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
(x1, y1, z1) <+> (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

(<->) :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
(x1, y1, z1) <-> (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)

(<*>) :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
(x1, y1, z1) <*> (x2, y2, z2) = (x1*x2, y1*y2, z1*z2)

(*>) :: (Double, Double, Double) -> Double -> (Double, Double, Double)
(x1, y1, z1) *> f = (x1*f, y1*f, z1*f)

maxF :: Double -> (Double, Double, Double) -> (Double, Double, Double)
maxF f (x,y,z) = (max x f, max y f, max z f)

minF :: Double -> (Double, Double, Double) -> (Double, Double, Double)
minF f (x,y,z) = (min x f, min y f, min z f)

(*.) :: Vector -> Vector -> Double
(x1, y1, z1) *. (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

len :: Vector -> Double
len v = sqrt (v *. v)

norm :: Vector -> Vector
norm v
  | len v < 10**(-9) = (0.0, 0.0, 0.0)
  | otherwise = v *> (1 / (len v))

mkNormVect :: Point3D -> Point3D -> Vector
mkNormVect v w = norm (w <-> v)

dist :: Point3D -> Point3D -> Double
dist p0 p1 = sqrt ((p1 <-> p0) *. (p1 <-> p0))

clip :: (Double, Double, Double) -> (Double, Double, Double)
clip = (maxF 0.0) . (minF 1.0)

solveq :: (Double, Double, Double) -> [Double]
solveq (a,b,c)
  | (d < 0) = [ ]
  | (d > 0) = [(-b - sqrt d)/(2*a), (-b + sqrt d)/(2*a)]
  | otherwise = [-b / (2*a)]
  where
    d = b*b - 4*a*c

-- why is this p1 and p2 and before it was p0 and p1?
mkRay :: Point3D -> Point3D -> Ray
mkRay p1 p2 = Ray p1 (mkNormVect p1 p2)

intersectionWithRay :: Ray -> Object -> [Double]
intersectionWithRay (Ray start dir) (Sphere radius center) =
  solveq (dir *. dir, 2*(dir *. d), (d *. d) - radius**2)
  where
    d = start <-> center
intersectionWithRay (Ray start dir) (Plane (a,b,c,d)) =
  if (abs(part) < 10**(-9)) then
    []
  else
    [-(d + ((a,b,c) *. start)) / part]
  where
    part = (a,b,c) *. dir

normal :: Point3D -> Object -> Vector
normal p (Sphere radius center) = norm ((p <-> center) *> (1/radius))
normal _ (Plane (a,b,c,_d)) = norm (a,b,c)

reflectDir :: Vector -> Vector -> Vector
reflectDir i n = i <-> (n *> (2*(n *. i)))

refractDir :: Vector -> Vector -> Double -> Vector
refractDir i n r =
  if (v < 0) then
    (0.0, 0.0, 0.0)
  else
    norm $ (i *> r_c) <+> (n *> (r_c*(abs c) - sqrt v))
  where
    c = n *. (i *> (-1))
    -- when cosVal < 0, inside of sphere (so travelling to vacuum)
    r_c = if (c < 0) then r else 1/r
    v = 1 + (r_c**2) * (c**2 - 1)

mapToWindow :: Resolution -> Dimension -> Point2D -> Point3D
mapToWindow (rx, ry) (width, height) (px, py) = (x/rxD, y/ryD, 0.0)
  where
    (rxD, ryD) = (fromIntegral rx, fromIntegral ry)
    (pxD, pyD) = (fromIntegral px, fromIntegral py)
    (widthD, heightD) = (fromIntegral width, fromIntegral height)
    (x, y) = ( (pxD - rxD/2)*widthD, (pyD - ryD/2)*heightD)

