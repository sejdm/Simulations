#!/usr/bin/env stack
-- stack --resolver lts-6.1 --install-ghc runghc --package simulate
{-# LANGUAGE FlexibleContexts #-}
import RealWorld3D
import Physics3D
import Diagrams.Prelude hiding (duration, radius, position)
import Simulation --(fromTop, simulateUsing, durationFun)
import Control.Monad
import Control.Monad.Trans
import AnimationCollection


-- The objects
sun = body & bobject .~ "sun"
           & bmass .~ 1.989*10^6
           & bradius .~ 24
           & bvelocity .~ (0.0, 0.0, 0.0)
           & bposition .~ (0, 0, 0)
           --_btoPicture = Standard,
           & btoPicture .~ External "sun.png"
           & bangularvelocity .~ 0.5



earth = body {
             _bobject = "earth",
             _bmass = 5.97,
             _bradius = 12,
             _bposition = (-147, 0, 0),
             _bvelocity = (0.0, 3000, 0.0),
             _bfragile = False,
             --_btoPicture = Standard,
             _btoPicture = External "earth.png",
             _bangularvelocity = 0.25
             }

horse = body {
              _bmass = 60,
              _bradius = 12,
              _bposition = (0, 250, -20),
              _bvelocity = (150, 0, 130),
              _btoPicture = Timevarying {
                  twidth = 60,
                  tfps = 30,
                  tdigits = Nothing,
                  tstart = 0,
                  tloop = 8,
                  tsuffix = ".png",
                  tprefix = "horse/horse-"
                  },
              _bangularvelocity = 0.25
              }

cartwheel = body {
               _bmass = 6,
               _bradius = 12,
               _bposition = (0, 150, -20),
               _bvelocity = (140, 0, 30),
               _btoPicture = Timevarying {
                   twidth = 60,
                   tfps = 30,
                   tdigits = Nothing,
                   tstart = 0,
                   tloop = 32,
                   tsuffix = ".png",
                   tprefix = "cartwheel/cartwheel-"
                   },
               _bangularvelocity = 0.25
               }

volcano = body {
               _bmass = 60,
               _bradius = 160,
               _bposition = (0, 250, -30),
               _bvelocity = (150, 0, -5),
               _btoPicture = Timevarying {
                   twidth = 160,
                   tfps = 30,
                   tdigits = Just 3,
                   tstart = 1,
                   tloop = 621,
                   tsuffix = ".png",
                   tprefix = "volcano/volcano-"
                   },
               _bangularvelocity = 0.25
               }

starryBackground = Timevarying {
                    twidth = 160,
                    tfps = 30,
                    tdigits = Nothing,
                    tstart = 1,
                    tloop = 621,
                    tsuffix = ".png",
                    tprefix = "twinkling/twinkling-"
                    }


friends = Timevarying {
                    twidth = 60,
                    tfps = 30,
                    tdigits = Nothing,
                    tstart = 500,
                    tloop = 10000,
                    tsuffix = ".png",
                    tprefix = "friends/friends"
                    }

earth2 = earth {_bobject = "earth2", _btoPicture = Internal (circle 20 # fc white)}
moon = body {
              _bobject = "moon",
              _bmass = 10,
              _bradius = 3,
              --_bfragile = True,
              _bposition = (0.00001, 0.00001, -147.37),
              _bvelocity = (180, 20, 0.00001)
              }

pivot = body {
  _bobject = "pivot",
  _bfixed = True,
  _bposition = (-100, 0, -10),
  _bmass = 10
             }
ball n = body {_bradius = 10, _bposition = (50*n, 0, 0), _bvelocity = (-20*n, 0, 0.0000)}
ball1 = body {_bradius = 20}
ball4 = ball1 {_bposition = (30, 30, -10)}
ball5 = ball1 {_bposition = (-30, -30, -30)}

ball2 = ball1 {
             _bposition = (500,0, -10),
             _bvelocity = (-40,0, -20)
             }

ball3 = ball1 {
              _bposition = (0, 100, -10),
              _bvelocity = (0, -20, -20)
              }
-- The systems
centripetalSystem = system {
                           --bodies = [pivot {_bfixed = True}, earth2],
                           bodies = [],
                           scaleDistancesBy = 2,
                           --viewFrom = fromTop,
                           internalField = centripetal
                           }
fixSun x | _bobject x == "earth" = x {_bfixed = True}
         | otherwise = x
fixAllSun (xs, y) = (map fixSun xs, y)

alterMass x = x {_bmass = _bmass x * 0.5 }
solarSystem = system {
                --bodies = [sun, earth],
                bodies = [sun {_btoPicture = Standard}, earth {_btoPicture = Standard}],
                internalField = gravitationField,
                --backgroundImage = starryBackground
                backgroundImage = Standard,
                scaleDistancesBy = 2,
                scaleTimeBy = 1,
                --viewFrom = fromTop,
                originBy = following "sun"
                }



simpleHarmonic = system {
                            bodies = [pivot, earth2],
                            internalField = centripetal
                            }

--collisionSystem = system {bodies = map ball [-5..5]}
collisionSystem = system {bodies = [ball 1, ball (-1)]}

fallingSystem = system {bodies = [ball1 {_bposition = (0,200, -20), _bvelocity = (0,0, 10)}], externalField = falling}

solarToCentri =
   (physicalScript solarSystem)
    {durationFun = 1, filterFun = clockExceeded 0.5} `simulateUsing` def {stretchTime = 0.1}
   >=> transformUsing fixAllSun
   >=> physicalScript centripetalSystem  `simulateUsing` def {stretchTime = 0.1}
--main = simulatePhysical def {trail= False, stretchTime = 1.00, trailRate = 1, duration = 5*60, enhancement = 10000, fps = 60} solarSystem

solToCent = do
  putSetting def {stretchTime = 0.1}
  phy solarSystem {stopWhen = clockExceeded 0.1}
  wait 2
  phy solarSystem

isVertical x = last (x ^. _1) ^.  bposition._1 >= 0


solToCent2 = do
  setPhysicalScript solarSystem
  stretchTimeBy .= 0.01
  addAnimAt (-400, 100) clock
  setAsDefault -- current state is set as default

  runBefore 1 isVertical -- run until 1 sec prior to becoming vertical
  animAt (-400, 300) (textBox "Oh no! Going to be vertical!!")
  p <- use img -- record state of the image rendering function at this stage

  runTill isVertical -- run till vertical
  logTime "Vertical"
  animAt (-400, 300) (textBox "Yes Now vertical!!")

  theBodies.traverse.btoPicture .= Folder "/home/shane/haskellStacks/simulate/images"

  runFor 4 -- run for 4 seconds
  logTime "4 seconds"
  internalFieldLens .= noForce
  animAt (-400, 300) (textBox "No more force, byeeee!!")
  theBodies.traverse.bmass *= 1/0.9

  runFor 4

  img .= p -- restore the recorded image function
  animAt (-400, 300) (textBox "All restored!")
  runFor 1

main = runSimSets solToCent2 (initPhy solarSystem)


--main = (physicalScript solarSystem `simulateUsing` def {trail= False, stretchTime = 0.01, trailRate = 1, duration = 5*60, enhancement = 10000, fps = 60}) (initPhysical solarSystem)

