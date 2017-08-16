module Output (getImage, createPPM) where

import Engine (Resolution, Scene, Color, rayTrace)

getImage :: Int -> Resolution -> Scene -> [Color]
getImage recDepth res@(rx, ry) scene =
  [image (fromIntegral x, fromIntegral (-y))
  | y <- [-(ry-1)..0], x <- [0..(rx-1)]]
  where
    image = rayTrace recDepth res scene

createPPM :: Resolution -> [Color] -> String
createPPM (w,h) colors =
  ("P3\n"++) . shows w . (' ':) . shows h . ("\n255\n"++)
  . stringify colors $ ""
  where stringify = flip $ foldr showC
        showC (r,g,b) = shows (round (r*255)) . (' ':) . shows (round (g*255)) 
	  . (' ':) . shows (round (b*255)) . (' ':)
