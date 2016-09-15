{-# LANGUAGE NoMonomorphismRestriction #-}
module Physics3D (Position, Velocity, Acceleration, Force, Momentum, Mass, Charge, State, Field, noField, noPotential, getState, collision',
                Newtonian (..), Electrical (..), RigidBody (..),
                 massToRadius, radiusToMass,
                 collision,
                 combineFields,  combineEffects, potentialToField, combinePotentials, combinePotentialEffects, fieldToEffect, scaleTimeInField,
                 gravitationPotential, gravitationField, gravity, centripetal, simpleHarmonic, pendulum, falling, noForce, potentialEnergy, centripetalExt, applyAcceleration
                 ) where


import Control.DeepSeq
import Data.List
import Vector3D
import Simulation (ToDouble (..), fromDouble)



type Position = (RealNumber, RealNumber, RealNumber)
type Velocity = (RealNumber, RealNumber, RealNumber)
type Acceleration = (RealNumber, RealNumber, RealNumber)
type Force = (RealNumber, RealNumber, RealNumber)
type Momentum = (RealNumber, RealNumber, RealNumber)
type Mass = RealNumber
type Charge = RealNumber
type State = (Position, Velocity)
type Field = State -> Acceleration
--type ExternalField = Field
type Potential = Position -> RealNumber


eqProp :: Eq b => (a -> b) -> a -> a -> Bool
eqProp f x y = f x == f y

scaleTimeInField :: RealNumber -> Field -> Field
scaleTimeInField t f = ((t^2) #*) . f


class Newtonian a where
  mass :: a -> RealNumber
  position :: a -> Position
  velocity :: a -> Velocity

  colliding :: a -> a -> Bool

  updateState :: State -> a -> a

  fixed :: a -> Bool
  fixed = const False

class Newtonian a => Electrical a where
  charge :: a -> Charge

class Newtonian a => RigidBody a where
  angularPosition :: a -> RealNumber
  angularVelocity :: a -> RealNumber




getState :: Newtonian a => a -> State
getState x = (position x, velocity x)

collision :: Newtonian a => a -> a -> a
collision o1 o2
    | colliding o1 o2 = updateState (if fixed o1 then (p2, neg vh2 #+ vv2)
                    else (p2, ((((m2 - m1) #* vh2) #+ ((2 * m1) #* vh1)) #/ (m1 + m2)) #+ vv2)) o2
    | otherwise = o2
    where vh1 = velocity o1 `proj` dir p1 p2
          vh2 = velocity o2 `proj` dir p1 p2
          vv2 = velocity o2 `proj` orthArg' (velocity o1) (velocity o2)
          p1 = position o1
          p2 = position o2
          m1 = mass o1
          m2 = mass o2



collision' :: Newtonian a => a -> a -> Velocity
collision' o1 o2
     | velocity o1 == (0,0,0) || velocity o2 == (0, 0, 0) = (0,0,0)
     | colliding o1 o2 = if fixed o1 then neg vh2 #+ vv2
                     else ((((m2 - m1) #* vh2) #+ ((2 * m1) #* vh1)) #/ (m1 + m2)) #+ vv2 #- velocity o2
     | otherwise = (0, 0, 0)
     where vh1 = velocity o1 `proj` dir p1 p2
           vh2 = velocity o2 `proj` dir p1 p2
           vv2 = velocity o2 `proj` orthArg' (velocity o1) (velocity o2)
           p1 = position o1
           p2 = position o2
           m1 = mass o1
           m2 = mass o2

combineFields :: [Field] -> Field
combineFields fs s = foldl1' (\x y -> force (x #+ y)) [ f s | f <- fs]

combineEffects :: [a -> Field] -> a -> Field
combineEffects fs x = combineFields [f x | f <- fs]

potentialToField :: Potential -> Field
potentialToField f (p, _) = grad f  p

combinePotentials :: [Potential] -> Potential
combinePotentials fs x = sum [f x | f <- fs]

combinePotentialEffects :: [a -> Potential] -> a -> Potential
combinePotentialEffects fs x = combinePotentials [f x | f <- fs]








massToRadius :: Mass -> RealNumber
massToRadius m = (fromDouble $ sqrt 10) * (fromDouble $ sqrt $ toDouble m)


radiusToMass :: RealNumber -> Mass
radiusToMass r = r^(2 :: Int)/10




fieldToEffect :: Newtonian a => (a -> Field) -> a -> a -> Acceleration
fieldToEffect f x y = f x (getState y)





gConst :: RealNumber
gConst = 6.67408 * 10^2

gravitationPotential :: Newtonian a => a -> Potential
gravitationPotential o1 p2 = gConst * m1 / dist p1 p2
        where m1 = mass o1
              p1 = position o1


gravitationField :: Newtonian a => a -> Field
gravitationField o1 (p2, _) = ((gConst * m1) / (dist p1 p2 ^(2 :: Int)) ) #* dir p2 p1
        where m1 = mass o1
              p1 = position o1

centripetal :: Newtonian a => a -> Field
centripetal o1 (p2, v2) = (((v `dot` v) / dist p1 p2) #* dir p2 p1 ) #- vn
                where p1 = position o1
                      v = (v2 #- velocity o1) `proj` orthDir p2 p1
                      vn = (v2 #- velocity o1) `proj` dir p1 p2


centripetalExt :: Field
centripetalExt (p2, v2) = (((v `dot` v) / dist p1 p2) #* dir p2 p1 ) #- vn
                 where p1 = (0,0,0)
                       v = (v2 #- (0,0,0)) `proj` orthDir p2 p1
                       vn = (v2 #- (0,0,0)) `proj` dir p1 p2

simpleHarmonic :: Newtonian a => a -> Field
simpleHarmonic o1 (p2, _)  = ((-10) * dist p1 p2) #* dir p1 p2
        where p1 = position o1

noForce :: Newtonian a => a -> Field
noForce _ _ = (0, 0, 0)

falling :: Field
falling _ = (0, -32, 0)

potentialEnergy :: Potential
potentialEnergy p = -8 * norm p

pendulum :: Newtonian a => a -> Field
pendulum o1 (p2, _) = fl #+ proj fl (dir p1 p2)
    where p1 = position o1
          fl = (0, -20, 0)


gravity :: Newtonian a => a -> a -> Force
gravity o1 o2 = mass o2 #* gravitationField o1 (getState o2)


noField :: Field
noField _ = (0, 0, 0)

noPotential :: Potential
noPotential _ = 0


applyAcceleration :: Newtonian a => RealNumber -> Acceleration -> a -> a
applyAcceleration h a b = if fixed b then b else updateState (p1 #+ (h #* v1), v1 #+ (h #* a)) b
   where (p1, v1) = getState b



