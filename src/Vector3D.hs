{-# LANGUAGE NoMonomorphismRestriction #-}
module Vector3D (RealNumber, Vector3D,
                 (#+), (#-), (#*), (#/), neg, dot, norm, dist, arg, orthArg, dir, orthDir, proj, grad, orthArg', orthDir') where
import Simulation (RealNumber, ToDouble (..), fromDouble)

type Point = (RealNumber, RealNumber, RealNumber)

type Vector3D = (RealNumber, RealNumber, RealNumber)



(#+) :: Vector3D -> Vector3D -> Vector3D
(x1, y1, z1) #+ (x2, y2, z2) = (x1+ x2, y1+y2, z1+z2)

(#-) :: Vector3D -> Vector3D -> Vector3D
(x1, y1, z1) #- (x2, y2, z2) = (x1- x2, y1-y2, z1-z2)

(#*) :: RealNumber -> Vector3D -> Vector3D
c #* (x, y, z) = (c*x, c*y, c*z)

(#/) :: Vector3D -> RealNumber -> Vector3D
v #/ c = (1.0 / c) #* v

neg :: Vector3D -> Vector3D
neg v = (-1) #* v

dot :: Vector3D -> Vector3D -> RealNumber
(x1, y1, z1) `dot` (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

norm :: Vector3D -> RealNumber
norm v = fromDouble $ sqrt  $ toDouble $ v `dot` v

arg :: Vector3D -> Vector3D
arg v = v #/ norm v

orthArg :: Vector3D -> Vector3D
orthArg (x, y, z) = arg (-y, x, z)

dist :: Point -> Point -> RealNumber
dist p1 p2 = norm (p2 #- p1)

dir :: Point -> Point -> Vector3D
dir v w = arg (w #- v)

orthDir :: Point -> Point -> Vector3D
orthDir v w = orthArg (w #- v)

proj :: Vector3D -> Vector3D -> Vector3D
proj v w = (v `dot` e) #* e where e = arg w

grad :: (Vector3D -> RealNumber) -> Vector3D -> Vector3D
grad f (x, y, z) = ((f (x + h, y, z) - f (x, y, z))/h, (f (x, y + h, z) - f (x, y, z))/h, (f (x, y, z + h) - f (x, y, z)))
    where h = 0.0001



orthArg' :: Vector3D -> Vector3D -> Vector3D
orthArg' (x, y, z) (x', y', z') = if v == (0, 0, 0) then (1, 1, 1) else v
  where v = arg (y*z' - z*y', - (x*z' - x'*z), x*y' - x'*y)

orthDir' :: Point -> Point -> Point -> Vector3D
orthDir' u v w = orthArg' (v #- u) (w #- u)
