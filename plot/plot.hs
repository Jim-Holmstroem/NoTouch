import Graphics.UI.GLUT
import Control.Monad
import Control.Arrow
import Control.Concurrent
import Data.IORef
import qualified System.IO as IO

import Data.Ord (comparing)
import qualified Safe
import qualified Text.Read as Read
import qualified Data.List as List
import qualified Data.List.Split as Split  -- split
import qualified Data.Map as Map

import qualified System.Hardware.Serialport as Serial


--(x',y') <- get pos
    --translate $ Vector3 x' y' 0
    --preservingMatrix $ do
    --    a <- get angle
    --    rotate a $ Vector3 0 0 1
    --    scale 0.7 0.7 (0.7 :: GLfloat)
    --    forM_ points $ \(x,y,z) -> preservingMatrix $ do
    --        color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
    --        translate $ Vector3 x y z
    --        cube 0.1



groupBy :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupBy key as = Map.fromListWith (++)  as' where
            as' = map ((,) <$> key <*> (:[])) as


vertex3f :: (GLfloat,GLfloat,GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z


testRect (x, y) (x', y') = do
    preservingMatrix $ do
        renderPrimitive Lines $ do
            color $ Color3 0 1 (0 :: GLfloat)
            vertex $ Vertex3 (realToFrac x ) (realToFrac y ) (0.0 :: GLfloat)
            vertex $ Vertex3 (realToFrac x') (realToFrac y') (0.0 :: GLfloat)
        renderPrimitive Lines $ do
            color $ Color3 1 0 (0 :: GLfloat)
            vertex $ Vertex3 (realToFrac x ) (realToFrac y') (0.0 :: GLfloat)
            vertex $ Vertex3 (realToFrac x') (realToFrac y ) (0.0 :: GLfloat)
        renderPrimitive LineLoop $ do
            color $ Color3 0 0 (1 :: GLfloat)
            vertex $ Vertex3 (realToFrac x ) (realToFrac y ) (0.0 :: GLfloat)
            vertex $ Vertex3 (realToFrac x ) (realToFrac y') (0.0 :: GLfloat)
            vertex $ Vertex3 (realToFrac x') (realToFrac y') (0.0 :: GLfloat)
            vertex $ Vertex3 (realToFrac x') (realToFrac y ) (0.0 :: GLfloat)


render (Just serialData) = do
    let renderLine (Sample time _ _ value) (Sample time' _ _ value') = do
        renderPrimitive Lines $ do
            color $ Color3 1 1 (1 :: GLfloat)
            vertex $ Vertex3 (realToFrac time ) (realToFrac value ) (0.0 :: GLfloat)
            vertex $ Vertex3 (realToFrac time') (realToFrac value') (0.0 :: GLfloat)


    let (Just (Just minTime)) = fmap time $ Safe.minimumByMay (comparing time) serialData  -- TODO will the minimum be Nothing?
    let (Just (Just maxTime)) = fmap time $ Safe.maximumByMay (comparing time) serialData
    let (Just (Just minValue)) = fmap value $ Safe.minimumByMay (comparing value) serialData
    let (Just (Just maxValue)) = fmap value $ Safe.maximumByMay (comparing value) serialData

    --print (minTime, maxTime, minValue, maxValue)

    let pairs = zip (tail serialData) serialData

    preservingMatrix $ do
        scale (realToFrac $ 1/(maxTime-minTime)) (realToFrac $ 1/(maxValue-minValue)) (1.0 :: GLfloat)
        translate $ Vector3 (realToFrac (-1*minTime)) (realToFrac (-1*minValue)) (0.0 :: GLfloat)
        --testRect (minTime,minValue) (maxTime,maxValue)

        zipWithM_ renderLine (tail serialData) serialData

render Nothing = return ()


display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef [Sample] -> DisplayCallback
display angle pos serialData = do
    clear [ColorBuffer]
    loadIdentity

    sd <- get serialData
    let zero = Map.lookup (Just (3, 4)) $ groupBy measurePoint sd

    render zero

    swapBuffers


idle :: IORef GLfloat -> IORef GLfloat -> DisplayCallback
idle angle delta = do
    d <- get delta
    angle $~! (+ d)
    postRedisplay Nothing


reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)


keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse a p key Down _ _ = case key of
    (Char ' ') -> a $~! negate
    (Char '+') -> a $~! (* 2)
    (Char '-') -> a $~! (/ 2)
    (SpecialKey KeyLeft) -> p $~! \(x, y) -> (x-0.1,y)
    (SpecialKey KeyRight) -> p $~! \(x, y) -> (x+0.1,y)
    (SpecialKey KeyUp) -> p $~! \(x, y) -> (x,y+0.1)
    (SpecialKey KeyDown) -> p $~! \(x, y) -> (x,y-0.1)
    _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()


data Sample = Sample Double Double Double Double | BadSample String
  deriving (Show)


isBad :: Sample -> Bool
isBad (Sample _ _ _ _) = False
isBad (BadSample _) = True


measurePoint :: Sample -> Maybe (Double, Double)
measurePoint (Sample _ sensor emitter _) = Just (sensor, emitter)
measurePoint (BadSample _) = Nothing

timeValue :: Sample -> Maybe (Double, Double)
timeValue (Sample time _ _ value) = Just (time, value)
timeValue (BadSample _) = Nothing

time (Sample time _ _ _) = Just time
time (BadSample _) = Nothing

sensor (Sample _ sample _ _) = Just sample
sensor (BadSample _) = Nothing

emitter (Sample _ _ emitter _) = Just emitter
emitter (BadSample _) = Nothing

value (Sample _ _ _ v) = Just v
value (BadSample _) = Nothing


sample :: String -> Sample
sample row = case Split.splitOn "," row of
    [time, sensor, emitter, value] -> maybeSample time' sensor' emitter' value'
        where maybeSample :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Sample
              maybeSample (Just time) (Just sensor) (Just emitter) (Just value) = Sample time sensor emitter value
              maybeSample Nothing _ _ _ = BadSample "Unreadable time"
              maybeSample _ Nothing _ _ = BadSample "Unreadable sensor"
              maybeSample _ _ Nothing _ = BadSample "Unreadable emitter"
              maybeSample _ _ _ Nothing = BadSample "Unreadable value"
              time' = Read.readMaybe time :: Maybe Double
              sensor' = Read.readMaybe sensor :: Maybe Double
              emitter' = Read.readMaybe emitter :: Maybe Double
              value' = Read.readMaybe value :: Maybe Double
    _ -> BadSample "Wrong number of values"


readData :: String -> IORef [Sample] -> IO ()
readData port serialData = do
    putStrLn "Starting reading serialdata"
    IO.withFile port IO.ReadMode $ \serial -> do
        cold <- replicateM 2 $ IO.hGetLine serial
        putStrLn $ "cold: " ++ show cold
        forever $ do
            sample <- fmap sample $ IO.hGetLine serial
            serialData $~! (sample:)


main :: IO ()
main = do
    (_progName, [port]) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    _window <- createWindow "Plot"

    serialData <- newIORef ([] :: [Sample])

    forkIO $ readData port serialData

    reshapeCallback $= Just reshape

    angle <- newIORef 0
    delta <- newIORef 0.1
    pos <- newIORef (0, 0)

    keyboardMouseCallback $= Just (keyboardMouse delta pos)
    idleCallback $= Just (idle angle delta)
    displayCallback $= display angle pos serialData

    mainLoop
