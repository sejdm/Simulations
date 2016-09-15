{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, TemplateHaskell, FlexibleContexts #-}
module RealWorld3D (
                 Body (..), body,
                 Image (..), obtain,
                 PhysicalSystem (..), system, simulatePhysical, physicalScript, initPhysical, clockExceeded, initPhy, phy,
                 modifyBodies, modifyEachBody, modifyBody,
                 internalFieldLens, theBodies, observerClock, backDrop,
                 setPhysicalScript,
                 fromInternal, fromExternal,
                 SimulationSettings (..),
                 placeAtIO, following, usingCOM, panning,
                 standardBackground,
                 timeBar, clockie,
                 bobject, bmass, bcharge, bradius, bposition, bvelocity, btoPicture, bexists, bangle, bangularvelocity, bfragile, bcollided, bexplosion, bcollidedAngle
                 ) where

import Diagrams.TwoD.Size
import Linear.Epsilon
--import Diagrams.Backend.Rasterific
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (norm, stretch, position, dot, radius, simulate, duration, Point)
import Control.DeepSeq
import Data.List
import Simulation
import Physics3D
import AnimationCollection
import Vector3D hiding (RealNumber)



--type Picture = Diagram B

data Image = Standard | Coloured (Colour Double) | Internal Picture | External FilePath | Folder FilePath | Timevarying {twidth :: RealNumber, tfps :: Int, tdigits :: Maybe Int, tstart :: Int, tloop :: Int,  tsuffix :: String, tprefix :: String}

instance Show Image where show = const "An image"

data Body a = Body {
        _bobject    :: a,
        _bmass      :: Mass,
        _bcharge    :: Mass,
        _bradius    :: RealNumber,
        _bfixed     :: Bool,
        _bposition  :: Position,
        _bvelocity  :: Velocity,
        _btoPicture :: Image,
        _bexists :: Bool,
        _bangle :: RealNumber,
        _bangularvelocity :: RealNumber,
        _bfragile :: Bool,
        _bcollided :: Bool,
        _bexplosion :: RealNumber,
        _bcollidedAngle :: RealNumber
       } deriving (Show)

makeLenses ''Body

vecToAngle :: (RealNumber, RealNumber, RealNumber) -> RealNumber
vecToAngle (v1, v2, _) = fromDouble $ atan (toDouble (negate v2 / v1)) / 2*pi

body :: Body ()
body = Body {
             _bobject = (),
             _bmass = 1,
             _bcharge = 0,
             _bradius = 1,
             _bfixed = False,
             _bposition = (0, 0, 0),
             _bvelocity = (0, 0, 0),
             _btoPicture = Standard,
             _bexists = True,
             _bangle =0,
             _bangularvelocity = 0,
             _bfragile = False,
             _bcollided = False,
             _bexplosion = 0,
             _bcollidedAngle = 0
            }

instance Newtonian (Body a) where
  mass = _bmass
  position = _bposition
  velocity = _bvelocity
  colliding x y = _bradius x + _bradius y >= dist (position x) (position y)
  updateState (p, v) b = p `seq` v `seq` b {_bposition = p, _bvelocity = v}

instance Electrical (Body a) where
  charge = _bcharge

instance Eq a => Eq (Body a) where
   x == y = all (\f -> f x y) [eqProp _bobject, eqProp mass, eqProp charge, eqProp _bradius, eqProp fixed, eqProp position, eqProp velocity, eqProp _bexists]


eqProp :: Eq b => (a -> b) -> a -> a -> Bool
eqProp f x y = f x == f y

instance NFData (Body a) where
  rnf (Body o m c r f p v t e a av fr bc ex bc') = bc' `seq` ex `seq` bc `seq` fr `seq` e `seq` o `seq` m `seq` c `seq` r `seq` f `seq` p `seq` a `seq` av `deepseq` v `deepseq` t `seq` Body o m c r f p v t `seq` ()

instance NFData (QDiagram a b c d) where
  rnf x = x `seq` ()

standardBackground :: Colour Double -> Picture
standardBackground c = (rect 1366 768 # fc c)

removeQuotes :: Show a => a -> String
removeQuotes = takeWhile (/= '"') . dropWhile (=='"') . show


finalPicture :: Show a => ObserverView -> Body a -> IO Picture
finalPicture v b | _bexplosion b <= 0 = rotateBy (toDouble $ _bangle b) <$> case _btoPicture b of
   Standard -> pure $ circle (toDouble $ _bradius b) # fc white
   Coloured c -> pure $ circle (toDouble $ _bradius b) # fc c
   Internal x -> pure x
   External s -> obtain (2 * (_bradius b)) s
   Folder s -> obtain (2 * (_bradius b)) (s ++ "/" ++ removeQuotes (_bobject b)++ ".png")
   Timevarying w fps Nothing s l su pr -> animateFile' w fps s l su pr (_secondsElapsed v)
   Timevarying w fps (Just n) s l su pr -> animateFile w fps n s l su pr (_secondsElapsed v)
                  | otherwise = (rotateBy (toDouble $ _bcollidedAngle b)) <$> animateFile' (10*_bradius b) 30 1 110 ".png" "explosion/explosion" (_bexplosion b)

finalBackground :: ObserverView -> IO Picture
finalBackground v = case _backgroundImg v of
    Standard -> pure $ standardBackground black
    Coloured c -> pure $ standardBackground c
    Internal x -> pure x
    External s -> obtain 1366 s
    Timevarying _ fps Nothing s l su pr -> animateFile' 1366 fps s l su pr (_secondsElapsed v)
    Timevarying _ fps (Just n) s l su pr -> animateFile 1366 fps n s l su pr (_secondsElapsed v)

obtain :: RealNumber -> FilePath -> IO Picture
obtain w fp = do
              res <- loadImageEmb fp
              return (let Right img = res in image img # sized (mkWidth $ toDouble w))

followBody :: Eq a => Maybe a -> [Body a] -> Position -> Position
followBody Nothing _ = id
followBody (Just x) bs = (#- _bposition b')
  where b' = head $ dropWhile ((/=x) . _bobject) bs

shiftOrigin :: Position -> Position -> Position
shiftOrigin (0,0,0) = id
shiftOrigin v = (#-v)

following :: Eq a => a -> ObserverTime -> [Body a] -> Position
following x _ = _bposition . head . dropWhile ((/=x) . _bobject)


com :: [Body a] -> Position
com bs = foldl1' (#+) (map (\b -> _bmass b #* _bposition b) bs) #/ sum  (map _bmass bs)


usingCOM :: ObserverTime -> [Body a] -> Position
usingCOM = const com


panning :: Velocity -> ObserverTime -> [Body a] -> Position
panning v t _ = t #* v

data Causes = Causes
              {
                acc :: Acceleration,
                impulse :: Velocity,
                collided :: Bool
              }

instance Monoid Causes where
  mappend c1 c2 = Causes {acc = (x, y, z), impulse = (v, w, w'), collided = collided c1 || collided c2}
    where
      !(!x, !y, !z) = (x1 + x2, y1 + y2, z1 + z2)
      !(!v, !w, !w') = (v1 + w1, v2 + w2, v3 + w3)

      (x1, y1, z1) = acc c1
      (v1, v2, v3) = impulse c1

      (x2, y2, z2) = acc c2
      (w1, w2, w3) = impulse c2

  mempty = Causes (0, 0, 0) (0, 0, 0) False

applyCause :: RealNumber -> Causes -> Body a -> Body a
applyCause t x b  | _bfixed b = b
                  | _bcollided b && _bfragile b =
                      b
                      {
                        _bexplosion = _bexplosion b + t,
                        _bvelocity = (0.0001, 0.0001, 0.0001),
                        _bexists = _bexplosion b < 4
                        }
                  | otherwise = applyAcceleration t (acc x) $
                    b
                    {
                      _bvelocity = impulse x #+ velocity b,
                      _bangle = _bangle b + t * _bangularvelocity b,
                      _bcollidedAngle = if collided x then vecToAngle (_bvelocity b) else _bcollidedAngle b,
                      _bcollided = collided x
                    }


onlyIfEffect :: (Body a -> Body a -> Causes) -> Body a -> Body a -> Causes
onlyIfEffect f b1 b2 | _bcollided b1 && _bfragile b1 = mempty
                     | otherwise = f b1 b2

fieldToCause :: Newtonian a => (a -> State -> Acceleration) -> a -> a -> Causes
fieldToCause f x y = Causes {acc = f x (getState y), impulse = collision' x y, collided = colliding x y}



externalToCause :: Newtonian a => (State -> Acceleration) -> a -> Causes
externalToCause f y = Causes {acc = f (getState y), impulse = (0, 0, 0), collided = False}

clockExceeded l (_, ObserverView t _) = t >= l

data ObserverView = ObserverView
                     {
                       _secondsElapsed :: !ObserverTime,
                       _backgroundImg :: Image
                     }
makeLenses ''ObserverView

observerClock :: Lens' (SimSets ([Body a], ObserverView)) Double
observerClock = model._2.secondsElapsed

backDrop :: Lens' (SimSets ([Body a], ObserverView)) Image
backDrop = model._2.backgroundImg

instance Show ObserverView where
   show x = show (_secondsElapsed x)

instance NFData ObserverView where
   rnf (ObserverView c b) = c `seq` b `seq` ObserverView c b `seq` ()



updateSecondsElapsed :: ObserverTime -> a -> ObserverView -> ObserverView
updateSecondsElapsed t _ v = v {_secondsElapsed = _secondsElapsed v + t}

{-
updateTrail :: RealNumber -> ObserverTime -> [Body a] -> ObserverView -> ObserverView
updateTrail n t x v = v
                     {
                      _secondsElapsed = _secondsElapsed v + t,
                      _backgroundImg = if
                         nearZero ((n * _secondsElapsed v) - (fromInteger $ round (n * _secondsElapsed v)))
                         then
                           coordinatePosition (finalBackground v) (\_ -> pure $ circle 5 # fc blue) _bposition x
                         else
                           _backgroundImg v
                     }

-}
updateTrail = undefined





data PhysicalSystem a = PhysicalSystem
                   {
                     bodies :: [Body a],
                     internalField :: Body a -> Field,
                     externalField :: Field,
                     backgroundImage :: Image,
                     follow :: Maybe a,
                     scaleDistancesBy :: RealNumber,
                     scaleTimeBy :: RealNumber,
                     originBy :: ObserverTime -> [Body a] -> Position,
                     viewFrom :: Position -> Position,
                     stopWhen :: ([Body a], ObserverView) -> Bool
                   }


system = PhysicalSystem {
                  bodies = [],
                  internalField = noForce,
                  externalField = noField,
                  backgroundImage = Standard,
                  follow = Nothing,
                  scaleDistancesBy = 1,
                  scaleTimeBy = 1,
                  originBy = const $ const (0, 0, 0),
                  viewFrom = id,
                  stopWhen = const False
                  }

physicalImage :: Show a => PhysicalSystem a -> (([Body a], ObserverView), RealNumber) -> IO Picture
physicalImage x ((os, v), _) = (coordinatePosition3D (finalBackground v) (finalPicture v) (viewFrom x . shiftOrigin (originBy x (_secondsElapsed v) os) . (scaleDistancesBy x #*) . _bposition) os)

setPhysicalScript :: (Eq a, Show a) => PhysicalSystem a -> SimState ([Body a], ObserverView) ()
setPhysicalScript x = do
  change .= l
  img .= p
  return ()
    where i = (bodies x, ObserverView 0 (backgroundImage x))
          l = (\t -> filter _bexists . interactionAndExternal applyCause causeChange causeChangeExternal t)
            `pairChange` (updateSecondsElapsed)
          p ((os, v), _) = (coordinatePosition3D (finalBackground v) (finalPicture v) (viewFrom x . shiftOrigin (originBy x (_secondsElapsed v) os) . (scaleDistancesBy x #*) . _bposition) os)
          -- p (os, v) = placeAtIO (-400, 300) (bird (_secondsElapsed v)) <$> coordinatePosition (_backgroundImg v) finalPicture _bposition os
          causeChange = onlyIfEffect (fieldToCause (scaleTimeInField (scaleTimeBy x) . internalField x))
          causeChangeExternal = externalToCause (scaleTimeInField (scaleTimeBy x) $ externalField x)

fromScaledInternalExternal st il ex = (\t -> filter _bexists . interactionAndExternal applyCause causeChange causeChangeExternal t)
             `pairChange` (updateSecondsElapsed)
  where
           causeChange = onlyIfEffect (fieldToCause (scaleTimeInField st . il))
           causeChangeExternal = externalToCause (scaleTimeInField st ex)

fromInternal :: (Eq a ) => (Body a -> Field) -> RealNumber -> ([Body a], ObserverView) -> ([Body a], ObserverView)
fromInternal i = fromScaledInternalExternal 1 i noField

fromExternal = fromScaledInternalExternal 1 noForce


physicalScript :: (Eq a, Show a) => PhysicalSystem a -> Script ([Body a], ObserverView)
physicalScript x = Script
  {
    transFun = l,
    imageFun = p,
    filterFun = stopWhen x,
    durationFun = 60
  }
   where i = (bodies x, ObserverView 0 (backgroundImage x))
         l = (\t -> filter _bexists . interactionAndExternal applyCause causeChange causeChangeExternal t)
           `pairChange` (updateSecondsElapsed)
         p ((os, v), _) = (coordinatePosition3D (finalBackground v) (finalPicture v) (viewFrom x . shiftOrigin (originBy x (_secondsElapsed v) os) . (scaleDistancesBy x #*) . _bposition) os)
         -- p (os, v) = placeAtIO (-400, 300) (bird (_secondsElapsed v)) <$> coordinatePosition (_backgroundImg v) finalPicture _bposition os
         causeChange = onlyIfEffect (fieldToCause (scaleTimeInField (scaleTimeBy x) . internalField x))
         causeChangeExternal = externalToCause (scaleTimeInField (scaleTimeBy x) $ externalField x)

phy s = simulateSt (physicalScript s)

initPhysical :: PhysicalSystem a -> IO (([Body a], ObserverView), Int)
initPhysical x = initial (bodies x, ObserverView 0 (backgroundImage x))

initPhy x = (bodies x, ObserverView 0 (backgroundImage x))

internalFieldLens = sets f'
   where f' h = over change (fromInternal . h)
   --where f' h = \x -> x {_change = fromInternal (h (_change x))}

theBodies = model._1


modifyBodies :: ([Body a] -> [Body a]) -> ([Body a], ObserverView) -> ([Body a], ObserverView)
modifyBodies h = (\(x, v) -> (h x, v))

modifyEachBody :: (Body a -> Body a) -> ([Body a], ObserverView) -> ([Body a], ObserverView)
modifyEachBody f = modifyBodies (map f)

modifyBody :: Eq a => a -> (Body a -> Body a) -> ([Body a], ObserverView) -> ([Body a], ObserverView)
modifyBody n f = modifyEachBody g
  where g x = if _bobject x == n then f x else x

simulatePhysical' :: (Eq a, Show a) => SimulationSettings ->
                      PhysicalSystem a -> (([Body a], ObserverView), Int)-> IO (([Body a], ObserverView), Int)
simulatePhysical' s x = simulate'' s (physicalImage x) l
   where
         l = (\t -> filter _bexists . interactionAndExternal applyCause causeChange causeChangeExternal t)
           `pairChange` (if trail s then updateTrail (trailRate s) else updateSecondsElapsed)
         -- p (os, v) = placeAtIO (-400, 300) (bird (_secondsElapsed v)) <$> coordinatePosition (_backgroundImg v) finalPicture _bposition os
         causeChange = onlyIfEffect (fieldToCause (scaleTimeInField (scaleTimeBy x) . internalField x))
         causeChangeExternal = externalToCause (scaleTimeInField (scaleTimeBy x) $ externalField x)


simulatePhysical :: (Eq a, Show a) => SimulationSettings ->
                    PhysicalSystem a -> IO (([Body a], ObserverView), Int)
simulatePhysical s x = simulate' s p l i
  where i = (bodies x, ObserverView 0 (backgroundImage x))
        l = (\t -> filter _bexists . interactionAndExternal applyCause causeChange causeChangeExternal t)
          `pairChange` (if trail s then updateTrail (trailRate s) else updateSecondsElapsed)
        p ((os, v), _) = (coordinatePosition3D (finalBackground v) (finalPicture v) (viewFrom x . shiftOrigin (originBy x (_secondsElapsed v) os) . (scaleDistancesBy x #*) . _bposition) os)
        -- p (os, v) = placeAtIO (-400, 300) (bird (_secondsElapsed v)) <$> coordinatePosition (_backgroundImg v) finalPicture _bposition os
        causeChange = onlyIfEffect (fieldToCause (scaleTimeInField (scaleTimeBy x) . internalField x))
        causeChangeExternal = externalToCause (scaleTimeInField (scaleTimeBy x) $ externalField x)



clockie :: (([Body a], ObserverView),RealNumber) -> IO Picture
clockie (x, _) = return $ stopClock (x ^. _2.secondsElapsed )

placeAt :: Position -> Diagram B -> Diagram B -> Diagram B
placeAt (x, y, z) p = ((translateX (toDouble x) . translateY (toDouble y)) p <>)


placeAtIO :: Position -> IO Picture -> IO Picture -> IO Picture
placeAtIO (x, y, z) p q = (\a b -> (translateX (toDouble x) $ translateY (toDouble y) a) <> b) <$> p <*> q
