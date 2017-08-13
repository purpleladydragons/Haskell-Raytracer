module Engine where

import Data.Maybe
import Math

type Color = (Double, Double, Double)

data Diff = Solid Color
  | Perlin (Point3D -> Color)

data Texture = Texture Diff Double Int Double Double

type TexturedObject = (Object, Texture)

type Intensity = (Double, Double, Double)

data Light = PointLight Point3D Intensity
  | AmbientLight Intensity

data Camera = Camera Point3D Dimension

data Scene = Scene Camera Color [TexturedObject] [Light]

data Intersection = Intersection Double Ray TexturedObject

type Image = Point2D -> Color

intDist :: (Maybe Intersection) -> Double
intDist Nothing = 0.0
intDist (Just (Intersection d _ _)) = d
intText :: (Maybe Intersection) -> Texture
intText Nothing = Texture (Solid (0.0, 0.0, 0.0)) 0.0 0 0.0 0.0
intText (Just (Intersection _ _ (_, t))) = t
colorAt :: (Maybe Intersection) -> Color
colorAt Nothing = (0.0, 0.0, 0.0)
colorAt (Just (Intersection _ _ (_, Texture (Solid color) _ _ _ _ ))) = color
colorAt i@(Just (Intersection _ _ (_, Texture (Perlin f) _ _ _ _ ))) = f (intPt i)
normalAt :: (Maybe Intersection) -> Vector
normalAt Nothing = (0.0, 0.0, 0.0)
normalAt i@(Just (Intersection _ _ (o, _) )) = normal (intPt i) o
intPt :: (Maybe Intersection) -> Point3D
intPt Nothing = (0.0, 0.0, 0.0)
intPt (Just (Intersection d (Ray start dir) _)) = start <+> (dir *> d)

firstPositive :: [Double] -> Double
firstPositive [] = 0.0
firstPositive (l:ls) = if l > 10**(-6) then l else firstPositive ls

closestInt :: Ray -> (Maybe Intersection) -> TexturedObject -> (Maybe Intersection)
closestInt ray intersection (object, texture) =
  if d > 10**(-6) && ((isNothing intersection) || d < (intDist intersection)) then
    Just (Intersection d ray (object, texture))
  else
    intersection
  where
    d = firstPositive (intersectionWithRay ray object)

intersect :: Ray -> [TexturedObject] -> (Maybe Intersection)
intersect ray objects = foldl (closestInt ray) Nothing objects

diff :: (Maybe Intersection) -> Light -> Color
diff _ (AmbientLight _) = (0.0, 0.0, 0.0)
diff i (PointLight pos int) = (int *> ((mkNormVect (intPt i) pos) *. (normalAt i))) <*> (colorAt i)

spec :: (Maybe Intersection) -> Vector -> Light -> Color
spec _ _ (AmbientLight _) = (0.0, 0.0, 0.0)
spec i d (PointLight pos int) = int *> (reflCoef * ( ((normalAt i) *. h) ** (fromIntegral specCoef)))
  where
    h = norm ((d *> (-1)) <+> (mkNormVect (intPt i) pos))
    (Texture _ reflCoef specCoef _ _) = intText i

shadePt :: Intersection -> Vector -> [TexturedObject] -> Light -> Color
shadePt _i _d _o (AmbientLight int) = int
shadePt i d o l@(PointLight pos _int)
  | s = (0.0,0.0,0.0)
  | otherwise = (diff (Just i) l) <+> (spec (Just i) d l)
  where
    s = not (isNothing i_s) && (intDist i_s) <= dist (intPt (Just i)) pos
    i_s = intersect (mkRay (intPt (Just i)) pos) o

reflectPt :: Int -> Intersection -> Vector -> [TexturedObject] -> [Light] -> Color
reflectPt depth i d = colorPt depth (Ray (intPt (Just i)) (reflectDir d (normalAt (Just i)))) (0.0,0.0,0.0)

refractPt :: Int -> Intersection -> Vector -> Color -> [TexturedObject] -> [Light] -> Color
refractPt depth i d b =
  if refractedDir == (0.0,0.0,0.0) then
    (\_x _y -> (0.0,0.0,0.0))
  else
    colorPt depth (Ray (intPt (Just i)) refractedDir) (b *> refrCoef)
  where
    refractedDir = refractDir d (normalAt (Just i)) refrIndex
    (Texture _ _ _ refrCoef refrIndex) = intText (Just i)

colorPt :: Int -> Ray -> Color -> [TexturedObject] -> [Light] -> Color
colorPt (-1) _ _ _ _ = (0.0, 0.0, 0.0)
colorPt d r@(Ray _ dir) b o l = if (isNothing i) then b else clip $ shadeColor <+> reflectColor <+> refractColor
  where
    shadeColor = foldl (<+>) (0.0,0.0,0.0) (map (shadePt (fromJust i) dir o) l)
    reflectColor =
      if (reflCoef == 0.0) then
        (0.0, 0.0, 0.0)
      else (
        reflectPt (d-1) (fromJust i) dir o l) *> reflCoef
    refractColor =
      if (refrCoef == 0.0) then
        (0.0, 0.0, 0.0)
      else
        (refractPt (d-1) (fromJust i) dir b o l) *> refrCoef
    i = intersect r o
    (Texture _ reflCoef _ refrCoef _) = intText i

rayTracePt :: Int -> Scene -> Point3D -> Color
rayTracePt d (Scene (Camera eye _) b o l) p = colorPt d (Ray p (mkNormVect eye p)) b o l

rayTrace :: Int -> Resolution -> Scene -> Image
rayTrace d r s@(Scene (Camera _ dim) _ _ _) = (rayTracePt d s) . (mapToWindow r dim)
