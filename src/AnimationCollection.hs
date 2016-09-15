module AnimationCollection
       (
         timeBar
         , stopClock
         , clock
         , bird
         , horse
         , cartwheel
       )
       where
import Simulation
import Diagrams.Prelude
--import Diagrams.Backend.Rasterific
import Diagrams.Backend.SVG

--type Picture = Diagram B


timeBar :: RealNumber -> Picture
timeBar t = rect (toDouble $ 10 * t) 30 # fc blue # translateX (toDouble $ 5*t)

clock :: (a, RealNumber) -> IO Picture
clock = usingTimePure stopClock

stopClock :: RealNumber -> Picture
stopClock t = hand 0.75 25 # rotateBy (-(toDouble $ t/3600)) <> hand 1 45 # rotateBy (-(toDouble $ t/60)) <> circle 50 # fc white
  where
    hand :: RealNumber -> RealNumber -> Diagram B
    hand w l = rect (toDouble w) (toDouble l) # fc blue # translateY (toDouble $ l/2)

bird :: RealNumber -> IO Picture
bird = animateFile 60 30 1 0 8 ".png" "bird-"


horse :: RealNumber -> IO Picture
horse = animateFile 60 30 1 0 8 ".png" "horse/horse-"

cartwheel :: RealNumber -> IO Picture
cartwheel = animateFile' 60 30 0 32 ".png" "cartwheel/cartwheel-"
