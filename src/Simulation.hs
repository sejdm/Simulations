{-# LANGUAGE BangPatterns, RankNTypes, NoMonomorphismRestriction, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}
--, FunctionalDependencies, UndecidableInstances, FlexibleInstances, ConstraintKinds, TypeFamilies #-}
module Simulation (
-- * A simulation module for simulation patterns

  -- $section1
  simulate, simulate', simulate'', ObserverTime, standardSimulate,
                  SimulationSettings (..), ToDouble (..), FromDouble (..), RealNumber, toRational',
                  Script (..), simulateUsing, transformUsing, initial, Sim, ScriptState,
                  simulateSt, runSimulate, putSetting, modifyObj, getObj, wait,
                  simulateSimSets, runSimSets, runTill, runFor, SimSets (..), SimState, stretchTimeBy, solutionFunction, lookAhead, runBefore,
                  setAsDefault, restoreDefault, restoreDefaults,
                  change, img, durationSeconds, stopIf, pause, framesPerSecond, model, addAnimAt, animAt, addPureAnimAt, usingTime, usingTimePure, textBox, logTime,
                  -- * The patterns
                  -- $section2
                  pairChange,
                  coordinatePosition, coordinatePosition3D,
                  --fromInteractions, useEffect,
                  interaction, effect, interactionAndExternal,
                  -- * Helper functions
                  -- $section3 images
                  animateFile, animateFile',
                  -- $section4 coordinate change
                  fromBack, fromBottom, fromFront, fromTop, fromLeft, fromRight, Picture
                  ) where


import Diagrams.TwoD.Size
import Diagrams.Combinators
--import Diagrams.Backend.Rasterific
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (norm, stretch, position, dot, radius, simulate, duration, Point)
import Control.DeepSeq
import Data.List
import Data.List.Split
import Control.Parallel.Strategies (using, parList, rseq)
import Control.Monad (zipWithM_)
import Data.Foldable
import GHC.Exts (Constraint)
import Control.Applicative
import qualified Control.Monad.Writer.Strict as W
import Control.Monad.State.Strict as S
import Control.Monad.Reader
import Control.Monad (join)
import Data.Ratio (Rational, Ratio)

renderRasterific = renderSVG

saveInFolder = "/tmp/ramdisk/"
--saveInFolder = "/home/shane/SimImgs"

type Picture = Diagram B
type ObserverTime = RealNumber

type RealNumber = Double
toRational' = fromDouble

class ToDouble a where
  toDouble :: a -> Double

instance ToDouble Double where
  toDouble = id

instance ToDouble Rational where
  toDouble = fromRational


class FromDouble a where
  fromDouble :: Double -> a

instance FromDouble Double where
  fromDouble = id

instance FromDouble Rational where
  fromDouble = toRational

wait' :: (NFData a, Show a) => SimulationSettings -> ((a, RealNumber) -> IO Picture) -> (a, Int) -> IO (a, Int)
wait' s f = simulate'' s{enhanceFactorancement = 1} f (const id)


generatePictures :: Show a => (a -> IO Picture) -> [a] -> IO ()
--generatePictures _ xs = mapM_ print xs
generatePictures f xs = zipWithM_ makeit [1000000..]  (map f xs `using` parList rseq)
   where
     makeit :: Int -> IO Picture -> IO ()
     makeit n x = do y <- x
                     renderRasterific (saveInFolder ++ show n ++ ".svg") (dims2D 1366 768) y

everyN :: NFData a => Int -> [a] -> [a]
everyN n xs  = map snd $ filter (\(m1,m2) -> m2 `deepseq` (m1 `mod` n == 0) ) $ zip [1..] xs
--everyN n xs  = map snd $ filter ((==n) . fst) (map force $ zip (cycle [1..n]) xs)

stretched :: RealNumber -> (RealNumber -> a -> a) -> RealNumber -> a -> a
stretched r f t = f (r*t)

{-
simulate' :: (NFData a, Show a) => SimulationSettings  -> (a -> IO Picture) -> (RealNumber -> a -> a) -> RealNumber -> a -> IO a
simulate' s mp f d x = 
   where fp = fps s
         en = enhanceFactorancement s
         helper n y = do z <- y
                         oneFrame n mp f (1/fromIntegral fp) z
-}


simulate'' :: (NFData a, Show a) => SimulationSettings  -> ((a, RealNumber) -> IO Picture) -> (RealNumber -> a -> a) -> (a, Int) -> IO (a, Int)
simulate'' s mp f (m, k) = do x <- simulateS fp en mp f' (fromIntegral d) (m, k)
                              return x
   where d = duration s
         fp = fps s
         en = enhanceFactorancement s
         sf = stretchTime s
         f' = stretched sf f

simulate' :: (NFData a, Show a) => SimulationSettings  -> ((a, RealNumber) -> IO Picture) -> (RealNumber -> a -> a) -> a -> IO (a, Int)
simulate' s mp f m = do x <- simulateS fp en mp f' (fromIntegral d) (m, 0)
                        return x
  where d = duration s
        fp = fps s
        en = enhanceFactorancement s
        sf = stretchTime s
        f' = stretched sf f


applyManyM :: (NFData a, Monad m) => Int -> (a -> m a) -> a -> m a
applyManyM 0 _ x = return x
applyManyM 1 f x = f x
applyManyM !n f x = do y <- f x
                       applyManyM (n - 1) f $!! y

applyMany :: NFData a => Int -> (a -> a) -> a -> a
applyMany 0 _ x = x
applyMany !n f x = applyMany (n - 1) f $!! f x

standardSimulate :: (Show a, NFData a) => ((a, RealNumber) -> IO Picture) -> (RealNumber -> a -> a) -> RealNumber -> (a, Int)  -> IO (a, Int)
standardSimulate = simulateS 30 1000

simulateS :: (Show a, NFData a) => Int -> Int -> ((a, RealNumber) -> IO Picture) -> (RealNumber -> a -> a) -> RealNumber -> (a, Int)  -> IO (a, Int)
simulateS fp en p f t = replicateS' n g t
  where n = floor $ fromIntegral fp * t
        g = oneFrame (timeToFramePic fp p) (enhanceFactorance en f)

replicateS' :: (NFData a, Monad m) => Int -> (RealNumber -> a -> m a) ->
               RealNumber -> a -> m a
replicateS' n f t = applyManyM n (f (t/fromIntegral n))

replicateS :: (NFData a, Monad m) => Int -> (RealNumber -> a -> m a) ->
              RealNumber -> a -> m a
replicateS n f t = replicateS'' n f (t/fromIntegral n)
  where replicateS'' 1 f h x  = f h x
        replicateS'' !m f h x = do y <- f h x
                                   y `deepseq` replicateS'' (m-1) f h y

timeToFramePic :: Int -> ((a, RealNumber) -> IO Picture) -> ((a, Int) -> IO Picture)
timeToFramePic fps p (x, n) = p (x, fromIntegral n/fromIntegral fps )

oneFrame :: (Show a, NFData a) => ((a, Int) -> IO Picture) ->
            (RealNumber -> a -> a) -> RealNumber -> (a, Int) -> IO (a, Int)
oneFrame p f t (x, m) = do let y = f t x
                           k <- p (x, m)
                           --print y
                           renderRasterific (saveInFolder ++ show (m + 10^6) ++ ".svg") (dims2D 1366 768) k
                           return (y, m+1)

enhanceFactorance :: NFData a => Int -> (RealNumber -> a -> a) -> RealNumber -> a -> a
enhanceFactorance n f t x = enhanceFactorance' n x
  where h = t/ fromIntegral n
        enhanceFactorance' 1 y  = force $ f h y
        enhanceFactorance' !m !y = enhanceFactorance' (m-1) $ force $ f h y

enhanceFactorance'' :: NFData a => Int -> (RealNumber -> a -> a) -> RealNumber -> a -> a
enhanceFactorance'' n f t x = applyMany n (f (t/ fromIntegral n)) x

-- | To use the default settings use `def`. For instance, to change only the frames-per-second to 16, use `def {fps = 16}`
data SimulationSettings = SimulationSettings
                          {
                            duration :: Int,
                            fps :: Int, -- ^ frames per second
                            enhanceFactorancement :: Int,
                            beginAt :: Int,
                            stretchTime :: RealNumber,
                            trail :: Bool,
                            trailRate :: RealNumber
                          }

instance Default SimulationSettings where
  def = SimulationSettings {duration = 60, fps = 30, enhanceFactorancement = 100000, beginAt = 0, stretchTime = 1, trail = False, trailRate = 1}


-- $section1
-- The main simulation functions

-- | Takes the simulation settings, a function that renders the simulation as an image and the simulation function to generate a series of images.
simulate :: (NFData a, Show a) => SimulationSettings
         -> (a -> IO Picture) -- ^ function to convert a value of type
                              -- @
                              -- a
                              -- @
                              -- into an image
         -> (RealNumber -> a -> a) -- ^ function 'f t x' defining change of `x` in time 't'.
         -> a -- ^ Initial value
         -> IO ()
simulate s mp f m = generatePictures mp (take (d*fp) $ drop (fp * beginAt s) $ everyN en $ iterate (f (1/fromIntegral (fp*en))) m)
  where d = duration s
        fp = fps s
        en = enhanceFactorancement s

monadifySimFun :: Monad m => (RealNumber -> a -> m a) -> RealNumber -> m a -> m a
monadifySimFun f t x = x >>= f t





applyManyB :: NFData a => Int -> (a -> Bool) -> (a -> a) -> a -> a
applyManyB 0 _ _ x = x
applyManyB !n b f x = if b x then x else applyMany (n - 1) f $!! f x

applyManyBM :: (NFData a, Monad m) => Int -> (a -> Bool) -> (a -> m a) -> a -> m a
applyManyBM 0 _ _ x = return $!! x
applyManyBM n b f x = do if b x
                           then return $!! x
                           else do y <- f x
                                   applyManyBM (n-1) b f $!! y

replicateBS' :: (NFData a, Monad m) => Int -> (a -> Bool) -> (RealNumber -> a -> m a) -> RealNumber -> a -> m a
replicateBS' n b f t = applyManyBM n b (f (t/fromIntegral n))


oneFrame' :: (Show a, NFData a) => ((a, Int) -> IO Picture) -> (RealNumber -> a -> a) -> RealNumber -> a -> IO a
oneFrame' p f t x = fst <$> oneFrame p f t (x, 0)

simulateBS :: (Show a, NFData a) => Int -> Int -> (a -> Bool) -> ((a, RealNumber) -> IO Picture) -> (RealNumber -> a -> a) -> RealNumber -> a -> IO a
simulateBS fp en b p f t = replicateBS' n b g t
  where n = floor $ fromIntegral fp * t
        g = oneFrame' (timeToFramePic fp p) (enhanceFactorance en f)

simulateBS' :: (Show a, NFData a) => Int -> Int -> (a -> Bool) -> ((a, RealNumber) -> IO Picture) -> (RealNumber -> a -> a) -> RealNumber -> (a, Int) -> IO (a, Int)
simulateBS' fp en b p f t = replicateBS' n b' g t
   where n = floor $ fromIntegral fp * t
         g = oneFrame (timeToFramePic fp p) (enhanceFactorance en f)
         b' (i, _) = b i

solveDiffEq :: (Show a, NFData a) => Int -> Int -> (RealNumber -> a -> a) -> RealNumber -> a -> a
solveDiffEq fp en f t = applyMany (floor $ fromIntegral (fp*en) * t) (f (1/ fromIntegral (fp*en)))

--simScript :: (Show a, NFData a) => Int -> Int -> (a -> Bool) -> (a -> IO Picture) -> (RealNumber -> a -> a) -> RealNumber -> Script a
--simScript fp en b p f t = StateT $ \x -> (x , simulateBS fp en b p f t x)
-- Sim as data


data Script a = Script {
  transFun :: RealNumber -> a -> a,
  imageFun :: (a, RealNumber) -> IO Picture,
  filterFun :: a -> Bool,
  durationFun :: RealNumber
               }
data SimSets a = SimSets
                 {
                   _framesPerSecond :: Int,
                   _enhanceFactor :: Int,
                   _change :: RealNumber -> a -> a,
                   _img :: (a, RealNumber) -> IO Picture,
                   _stopIf :: a -> Bool,
                   _durationSeconds :: RealNumber,
                   _stretchTimeBy :: RealNumber,
                   _currentFrameNumber :: Int,
                   _model :: a,
                   _defaultSimSets :: SimSets a
                 }

makeLenses ''SimSets

instance Default (SimSets a) where
  def = SimSets
    {
      _framesPerSecond = 30,
      _enhanceFactor = 1000,
      _durationSeconds = 1000,
      _currentFrameNumber = 0,
      _stopIf = always,
      _stretchTimeBy = 1,
      _defaultSimSets = def
    }

type SimState a = StateT (SimSets a) IO

setAsDefault :: (Show a, NFData a) => SimState a ()
setAsDefault = do x <- use id
                  defaultSimSets .= x

restoreDefault :: Lens' (SimSets a) b -> SimState a ()
restoreDefault l = do x <- use (defaultSimSets.l)
                      l .= x

restoreDefaults = restoreDefault id

simulateSimSets :: (Show a, NFData a) => SimState a ()
simulateSimSets = StateT $ \x -> do (y2, n2) <- simulateBS' (_framesPerSecond x) (_enhanceFactor x) (_stopIf x) (_img x) (stretched (_stretchTimeBy x) (_change x)) (_durationSeconds x) (_model x, _currentFrameNumber x)
                                    return $! ((), x {_currentFrameNumber = n2, _model =y2})

always = const False

runSimSets s i = runStateT s def {_model = i}

runFor t = durationSeconds .= t >> simulateSimSets

runTill b = stopIf .= b >> simulateSimSets >> stopIf .= always

solutionFunction = solveDiffEq <$> use framesPerSecond <*> use enhanceFactor <*> use change

lookAhead t = do f <- solutionFunction
                 return (f t)

runBefore t b = do f <- lookAhead t
                   runTill (b . f)

addAnimAt :: (Double, Double) -> ((a, RealNumber) -> IO Picture) -> SimState a ()
addAnimAt v g = img %= (\f -> (\t -> do p <- f t; s <- g t; return ( translate (r2 v) s <> p)))

animAt v g = do restoreDefault img
                addAnimAt v g

addPureAnimAt :: (Double, Double) -> ((a, RealNumber) -> Picture) -> SimState a ()
addPureAnimAt v g = img %= (\f -> (\t -> do p <- f t; return ( translate (r2 v) (g t) <> p)))


usingTime :: (RealNumber -> IO Picture) -> (a, RealNumber) -> IO Picture
usingTime p (_, t) = p t

usingTimePure :: (RealNumber -> Picture) -> (a, RealNumber) -> IO Picture
usingTimePure p (_, t) = return $ p t

textBox :: String -> (a, RealNumber) -> IO Picture
textBox s = return . const (text s # fc white # lc white # scale 20)

--runTill b = StateT $ \x -> do (y, n) <- simulateBS' (_framesPerSecond x) (_enhanceFactor x) b (_img x) (_change x) (_durationSeconds x) (_model x, _currentFrameNumber x)
                              --return $! (y, x {_currentFrameNumber = n, _model =y})

pause :: (Show a, NFData a) => Int -> SimState a ()
pause t = do
  p <- use img
  x <- use model
  n <- use currentFrameNumber
  (x2, n2) <- liftIO $ wait' def {duration = t} p (x, n)
  model .= x2
  currentFrameNumber .= n2
  return ()

secondsElapsed = lens ((/) <$> fromIntegral . view currentFrameNumber <*> fromIntegral . view framesPerSecond) (\x s -> set currentFrameNumber (floor (fromIntegral (x ^. framesPerSecond) * s)) x)

clockTime = secondsElapsed . to secondsToTime


logTime s = do t <- use secondsElapsed
               liftIO (putStrLn $ show t ++ ": " ++ s)


data MyTime = Time Int Int Int Double
instance Show MyTime where
  show (Time h m s d) = show h ++ ":" ++ show m ++ ":" ++ show s ++ "." ++ show d

secondsToTime t = Time h m s' (roundTo 2 t - fromIntegral s)
  where s = floor t
        h = s `div` 3600
        m = (s `mod` 3600) `div` 60
        s' = s `mod` 3600


timeToSeconds (Time h m s d) = fromIntegral (h*3600 + m*60 + s) + d


roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

type ScriptState a = StateT (SimulationSettings, (a, RealNumber) -> IO Picture, (a, Int)) IO

simulateB' :: (Show a, NFData a) => Script a -> (a, Int) -> SimulationSettings -> IO (a, Int)
simulateB' s x ss = simulateBS' (fps ss) (enhanceFactorancement ss) (filterFun s) (imageFun s) (transFun s) (durationFun s) x
   --where ss = settingsFun s


simulateSt :: (Show a, NFData a) => Script a -> ScriptState a a
simulateSt s = StateT $ \(ss, _, x) -> do y <- simulateUsing s ss x
                                          return (fst y, (ss, imageFun s, y))

runSimulate s x = runStateT s (def, const (return mempty), (x, 0))

putSetting :: SimulationSettings -> ScriptState a ()
putSetting ss = do (_, p, x) <- get
                   put (ss, p, x)
                   return ()

modifyObj f = do (ss, p, (x, n)) <- get
                 put (ss, p, (f x, n))
                 return ()

getObj = do (_, _, (x, _)) <- get
            return x

wait :: (Show a, NFData a) => Int -> ScriptState a ()
wait t = do
  (ss, p, x) <- get
  y <- liftIO $ wait' ss {duration = t} p x
  put (ss, p, y)
  return ()

simulateUsing :: (Show a, NFData a) => Script a -> SimulationSettings -> (a, Int) -> IO (a, Int)
simulateUsing s ss x = simulateBS' (fps ss) (enhanceFactorancement ss) (filterFun s) (imageFun s) (stretched (stretchTime ss) (transFun s)) (durationFun s) x
    --where ss = settingsFun s

transformUsing :: (a -> a) -> (a, Int) -> IO (a, Int)
transformUsing f (x, n) = return (f x, n)

simulateB :: (Show a, NFData a) => Script a -> a -> SimulationSettings -> IO a
simulateB s x ss = simulateBS (fps ss) (enhanceFactorancement ss) (filterFun s) (imageFun s) (transFun s) (durationFun s) x
  --where ss = settingsFun s

{-
combineScripts :: ((a, Int) -> (a, Int) -> Picture -> Picture -> Picture) -> Script a -> Script b -> Script (a, b)
combineScripts f (Script t1 i1 f1 d1) (Script t2 i2 f2 d2) =
  Script
  (\t (x, y) -> (t1 t x, t2 t y))
  (\(x,y) -> f x y <$> (i1 x) <*> (i2 y))
  (\(x, y) -> f1 x && f2 y)
  (min d1 d2)

animScript :: (RealNumber -> IO Picture) -> Script RealNumber
animScript f = Script (+) f (const False) 100000

inset :: Script a -> Script b -> Script (a, b)
inset = combineScripts f
  where f _ _ p1 p2 = p2 # scale 0.2 <> p1
 -}

type Sim a = SimulationSettings -> IO (a, Int)

initial x = return (x, 0)

(%%%) :: (Show a, NFData a) => Sim a -> Script a -> Sim a
x %%% s = \ss -> do y <- x ss
                    simulateB' s y ss

-- PART I - Simulation patterns





-- Some helper functions
fold' :: Monoid a => [a] -> a
fold' = foldl' mappend mempty

filterG :: (Applicative f, Foldable f, Monoid (f a)) =>
           (a -> Bool) -> f a -> f a
filterG p = foldMap (\a -> if p a then pure a else mempty)


-- The patterns

-- Foldable and Traversable patterns
effect :: (Applicative f, Foldable f, Monoid (f a), Eq a) =>
                     (ObserverTime -> f a -> a -> c) ->
                     ObserverTime -> f a -> f c
effect f t xs = fmap (\r -> f t (exceptOne r) r) xs
   where exceptOne o = filterG (/=o) xs

combineInteractions :: (Applicative f, Foldable f, Monoid (f a), Eq a, Monoid b) =>
                   (ObserverTime -> b -> a -> c) -> (a -> a -> b) ->
                   ObserverTime -> f a -> a -> c
combineInteractions c f t xs x = c t (fold (fmap f xs) x) x


combineInteractionsAndExternal ::
  (Applicative f, Foldable f, Monoid (f a), Eq a, Monoid b) =>
  (ObserverTime -> b -> a -> c) -> (a -> a -> b) ->
  (a -> b) ->
  ObserverTime -> f a -> a -> c
combineInteractionsAndExternal c f g t xs x = c t ((g `mappend` fold (fmap f xs)) x) x



interaction :: (Applicative f, Foldable f, Monoid (f a), Eq a, Monoid b) =>
              (ObserverTime -> b -> a -> c) -> (a -> a -> b) ->
              ObserverTime -> f a -> f c
interaction c f  = effect (combineInteractions c f)




interactionAndExternal :: (Applicative f, Foldable f, Monoid (f a), Eq a, Monoid b) =>
               (ObserverTime -> b -> a -> c) -> (a -> a -> b) ->
               (a -> b) ->
               ObserverTime -> f a -> f c
interactionAndExternal c f g  = effect (combineInteractionsAndExternal c f g)


interaction' :: (Alternative f, Applicative f, Foldable f, Monoid (f a), Eq a, Monoid b) =>
                (ObserverTime -> b -> a -> f a) -> (a -> a -> b) -> ObserverTime -> f a -> f a
interaction' c f t = asum . interaction c f t


--- Monadic versions
effectM :: (Applicative f, Traversable f, Foldable f, Monoid (f a), Eq a, Monad m) =>
                      (ObserverTime -> f a -> a -> m c) ->
                      ObserverTime -> f a -> m (f c)
effectM f t xs = sequence $ fmap (\r -> f t (exceptOne r) r) xs
    where exceptOne o = filterG (/=o) xs

effectM' :: (Applicative f, Traversable f, Foldable f, Monoid (f a), Eq a, Monad m) =>
                      (ObserverTime -> f a -> a -> m c) ->
                      ObserverTime -> m (f a) -> m (f c)
effectM' f t = (>>=effectM f t)

combineMonadicFuncs :: (Monad m, Monoid b, Traversable f, Foldable f) => f (a -> m b) -> a -> m b
combineMonadicFuncs xs x = fold <$> sequence (fmap ( $ x) xs)


monadicMappendFunc :: (Monad m, Monoid b) => (a -> m b) -> (a -> m b) -> a -> m b
monadicMappendFunc f g x = mappend <$> (f x) <*> (g x)


combineInteractionsM :: (Applicative f, Foldable f, Traversable f, Monoid (f a), Eq a, Monoid b, Monad m) =>
                    (ObserverTime -> b -> a -> m c) -> (a -> a -> m b) ->
                    ObserverTime -> f a -> a -> m c
combineInteractionsM c f t xs x =
  join (c t <$> (combineMonadicFuncs (fmap f xs) x) <*> pure x)


interactionM :: (Applicative f, Foldable f, Traversable f, Monoid (f a), Eq a, Monoid b, Monad m) =>
               (ObserverTime -> b -> a -> m c) -> (a -> a -> m b) ->
               ObserverTime -> f a -> m (f c)
interactionM c f  = effectM (combineInteractionsM c f)

interactionM' :: (Applicative f, Foldable f, Traversable f, Monoid (f a), Eq a, Monoid b, Monad m) =>
               (ObserverTime -> b -> a -> m c) -> (a -> a -> m b) ->
               ObserverTime -> m (f a) -> m (f c)
interactionM' c f t = (>>=interactionM c f t)

combineInteractionsAndExternalM ::
   (Applicative f, Foldable f, Traversable f, Monoid (f a), Eq a, Monoid b, Monad m) =>
   (ObserverTime -> b -> a -> m c) -> (a -> a -> m b) ->
   (a -> m b) ->
   ObserverTime -> f a -> a -> m c
combineInteractionsAndExternalM c f g t xs x = join (c t <$> ((g `monadicMappendFunc` combineMonadicFuncs (fmap f xs)) x) <*> pure x)



interactionAndExternalM :: (Applicative f, Foldable f, Traversable f, Monoid (f a), Eq a, Monoid b, Monad m) =>
                (ObserverTime -> b -> a -> m c) -> (a -> a -> m b) ->
                (a -> m b) ->
                ObserverTime -> f a -> m (f c)
interactionAndExternalM c f g  = effectM (combineInteractionsAndExternalM c f g)



interactionAndExternalM' :: (Applicative f, Foldable f, Traversable f, Monoid (f a), Eq a, Monoid b, Monad m) =>
                 (ObserverTime -> b -> a -> m c) -> (a -> a -> m b) ->
                 (a -> m b) ->
                 ObserverTime -> m (f a) -> m (f c)
interactionAndExternalM' c f g t = (>>= interactionAndExternalM c f g t)




interaction'M :: (Alternative f, Applicative f, Traversable f, Foldable f, Monoid (f a), Eq a, Monoid b, Monad m) =>
                 (ObserverTime -> b -> a -> m (f a)) -> (a -> a -> m b) -> ObserverTime -> f a -> m (f a)
interaction'M c f t xs = asum <$> interactionM c f t xs

interaction'M' c f t = (>>= interaction'M c f t)



-- $section2
-- Patterns that are possible for pairs

pairChange :: (ObserverTime -> a -> c) -> (ObserverTime -> a -> b -> d) ->
               ObserverTime -> (a, b) -> (c, d)
pairChange f g t (x, y) = (f t x, g t x y)


interactionAndExternalPair :: (Applicative f, Foldable f, Monoid (f a), Eq a, Monoid b) =>
                (ObserverTime -> b -> a -> c) -> (a -> a -> b) ->
                (a -> b) ->
                ObserverTime -> f a -> f (c, b)
interactionAndExternalPair c f g  = interactionAndExternal c' f g
  where c' t x y = (c t x y, x)

{-
interactionAndExternalPair :: (Applicative f, Foldable f, Monoid (f a), Eq a, Monoid b) =>
                (ObserverTime -> b -> a -> c) -> (ObserverTime -> b -> d -> d) -> (a -> a -> b) ->
                (a -> b) ->
                ObserverTime -> (f a, d) -> (f c, d)
interactionAndExternalPair c c' f g t (xs, s) = ((effect (combineInteractionsAndExternal c f g)) t xs, )
-}

-- Writer patterns
writerChange :: Monoid b =>
                (ObserverTime -> a -> a) -> (ObserverTime -> a -> b) ->
                ObserverTime -> W.Writer b a -> W.Writer b a
writerChange f g t w = do x <- w
                          W.tell (g t x)
                          return (f t x)



-- State patterns

stateChange :: (ObserverTime -> a -> a) -> (ObserverTime -> a -> s -> s) ->
               ObserverTime -> S.State s a -> S.State s a
stateChange f g t w = do x <- w
                         s' <- S.get
                         S.put (g t x s')
                         return (f t x)





-- PART II Image rendering

mytranslate (x, y) = translateX (checkNaN $ realToFrac x) . translateY (checkNaN $ realToFrac y)





checkNaN x = if isNaN x then error "is NaN" else x

positionPicture :: Real n => (a -> IO Picture) -> (a -> (n, n)) -> a  -> IO Picture
positionPicture f p o = (translateX (checkNaN $ realToFrac x1) . translateY (checkNaN $ realToFrac y1)) <$> i
                 where (x1, y1) = p o
                       i = f o

positionPicture3D :: Real n => (a -> IO Picture) -> (a -> (n, n, n)) -> a  -> IO Picture
positionPicture3D f p o = (scale (checkNaN $ ifZero $ 500/ (realToFrac z1 + 500)) . translateX (checkNaN $ realToFrac x1) . translateY (checkNaN $ realToFrac y1)) <$> i
                  where (x1, y1, z1) = p o
                        i = f o

ifZero x | abs x < 0.1 = error "Too close"
         | abs x > 10 = error "Too far"
         | otherwise = x


coordPos :: (a -> IO Picture) -> (p -> Picture -> Picture) -> (a -> p) -> [a] -> IO Picture
coordPos f g p xs = mconcat <$> mapM (\x -> g (p x) <$> f x) xs





coordinatePosition :: Real n => IO Picture -> (a -> IO Picture) -> (a -> (n, n)) -> ([a], Int) -> IO Picture
coordinatePosition b f p (xs, _) = (<>) <$> (mconcat <$> mapM (positionPicture f p) xs) <*> b


coordinatePosition3D :: Real n => IO Picture -> (a -> IO Picture) -> (a -> (n, n, n)) -> [a] -> IO Picture
coordinatePosition3D b f p xs = (<>) <$> (mconcat <$> mapM (positionPicture3D f p) ys) <*> b
    where ys = sortBy (\x y -> thd (p x) `compare` thd (p y)) $ reverse xs

thd (_, _, a) = a

gridPosition :: Int -> (a -> Picture) -> [a] -> IO Picture
gridPosition m f xs = pure $ vcat $ map hcat $ chunksOf m $ map f xs






-- PART III Extras



{-
changeOrigin :: (RealNumber, RealNumber) -> (RealNumber, RealNumber) -> (RealNumber, RealNumber)
changeOrigin (x0, y0) (x1, y1) = (x1 - x0, y1 - y0)

fixedOrigin :: (a -> (RealNumber, RealNumber)) -> [a] -> a -> (RealNumber, RealNumber)
fixedOrigin f _ = f


-- changes the origin to the first object that satisfies the condition in the first argument
originDefinedBy :: (a -> Bool) -> (a -> (RealNumber, RealNumber)) -> [a] -> a -> (RealNumber, RealNumber)
originDefinedBy c p os o = changeOrigin (p u) (p o)
  where u = head $ dropWhile (not . c) os




addTrail :: (Num n, Real n) => (a -> (n, n)) -> [a] -> IO Picture
addTrail c xs = coordPos (\_ -> pure (circle 1 # fc blue)) mytranslate c xs


updateTrail :: (Num n, Real n) => (a -> (n, n)) -> ObserverTime -> [a] -> ObserverView -> ObserverView
updateTrail f t xs v = v {clock = clock v + t, backgroundImg = (<>) <$> addTrail f xs <*> backgroundImg v}
-}

{-

fromInteractions :: Eq a => (ObserverTime -> [a] -> a -> c) -> ObserverTime -> [a] -> [c]
fromInteractions f t xs = map (\r -> f t (exceptOne r) r) xs
    where exceptOne o = filter (/=o) xs

crudeCombine :: (a -> a -> a) -> [a] -> a -> a
crudeCombine g xs y = foldl' (flip g) y xs

combineEffects :: Monoid b => (ObserverTime -> b -> a -> c) -> (a -> a -> b) -> (a -> a -> a) -> ObserverTime -> [a] -> a -> c
combineEffects c f g t xs y = c t (mconcat (map f xs) x) x
   where x = crudeCombine g xs y


useEffect :: (Monoid b, Eq a) => (ObserverTime -> b -> a -> c) -> (a -> a -> b) -> (a -> a -> a) -> ObserverTime -> [a] -> [c]
useEffect c f g  = fromInteractions (combineEffects c f g)


useEffect' :: (Monoid b, Eq a) => (ObserverTime -> b -> a -> [a]) -> (a -> a -> b) -> (a -> a -> a) -> ObserverTime -> [a] -> [a]
useEffect' c f g t = concat . useEffect c f g t

-}

filenames :: String -> String -> Int -> Int -> Int -> [String]
filenames su p n s e = map ((p ++) . (++su) . tail . show) [10^n+s, 10^n + s+ 1..10^n + e]

fileAnimate :: Int -> Int -> Int -> Int -> String -> String -> ObserverTime -> String
fileAnimate fps n s l su pr t = pr ++ (tail $ show ((floor (t * fromIntegral fps)) `mod` l +10^n + s)) ++ su


fileAnimate' :: Int -> Int -> Int -> String -> String -> ObserverTime -> String
fileAnimate' fps s l su pr t = pr ++ (show (((floor (t * fromIntegral fps)) `mod` l)+s)) ++ su


obtain :: RealNumber -> FilePath -> IO Picture
obtain w fp = do
               res <- loadImageEmb fp
               return (let Right img = res in image img # sized (mkWidth $ toDouble w))


-- | A helper function to be generate an argument of `simulate`
animateFile :: RealNumber -- ^ width
            -> Int -- ^ frames per second
            -> Int
            -> Int -- ^ begin with frame number
            -> Int -- ^ loop after
            -> String -- ^ file name suffix (eg. png)
            -> String -- ^ file name
            -> ObserverTime
            -> IO Picture
animateFile w fps n s l su pr t = obtain w (fileAnimate fps n s l su pr t)




-- | Like animateFile but with file format image002.png
animateFile' :: RealNumber -> Int -> Int -> Int -> String -> String -> ObserverTime -> IO Picture
animateFile' w fps s l su pr t = obtain w (fileAnimate' fps s l su pr t)


-- $section4
-- The following functions are to view the simulation from various angles.

fromFront :: Real n => (n, n, n) -> (n, n, n)
fromFront = id

fromLeft :: Real n => (n, n, n) -> (n, n, n)
fromLeft (x, y, z) = (-z, y, x)


fromRight :: Real n => (n, n, n) -> (n, n, n)
fromRight (x, y, z) = (z, y, -x)


fromTop :: Real n => (n, n, n) -> (n, n, n)
fromTop (x, y, z) = (x, z, -y)


fromBottom :: Real n => (n, n, n) -> (n, n, n)
fromBottom (x, y, z) = (x, -z, y)


fromBack :: Real n => (n, n, n) -> (n, n, n)
fromBack (x, y, z) = (-x, y, -z)

