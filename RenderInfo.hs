module RenderInfo where

import Engine (Scene(Scene), Camera(Camera), Texture(Texture),
                Light(PointLight), Diff(Solid))
import Math (Resolution, Object(Sphere,Plane))

data Render = Render Int Resolution Scene

getRenderInfo :: () -> Render
getRenderInfo () =
  let dimension = (200, 150)
      camera = Camera (0.0,0.0,1000.0) dimension
      bgColor = (0.8,0.8,0.8)
      sphere = Sphere (10.0) (30.0, 0.0, -20.0)
      otherSphere = Sphere (15.0) (-40.0, 5.0, -20.0)
      anotherSphere = Sphere (5.0) (0.0, 0.0, -50.0)
      texture = Texture (Solid (0.9, 0.7, 0.7)) 0.3 1 0.5 0.5
      otherTexture = Texture (Solid (1.0, 0.2, 0.2)) 0.8 1 0.1 0.5
      objects = [ (sphere, texture), (otherSphere, otherTexture)
                , (anotherSphere, texture)]
      lights = [ PointLight (0.0,00.0,100.0) (0.8,0.8,0.8) ]
      scene = Scene camera bgColor objects lights
  in
  (Render 4 (400, 300) scene)

