{-# LANGUAGE RecursiveDo #-}

import Control.Monad (when, join, unless, replicateM)
import Control.Monad.Fix (fix)
import Control.Concurrent (threadDelay)
import Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering
import Graphics.Gloss.Geometry.Angle
import FRP.Elerea.Simple
import System.Exit
import System.Random
import Data.Fixed (mod')
import Data.List (sort)

type Pos = Point
type Vel = (Float, Float)
type AngularVel = Float
type Rot = Float
type Size = Float

data Player = Player Pos Vel Rot

data Asteroid = Asteroid Path Size Pos Vel Rot AngularVel

initialPlayer = Player (0, 0) (0, 0) 0

playerAcceleration = 0.5
playerRotSpeed = 10
playerSize = 15

minAsteroidVel = 1
maxAsteroidVel = 3
rangeAsteroidVel = maxAsteroidVel - minAsteroidVel

minAsteroidSize = 20
maxAsteroidSize = 45
rangeAsteroidSize = maxAsteroidSize - minAsteroidSize

maxAsteroidAngularVel = 5

timeStep :: Int
timeStep = 1000000 `quot` 60

width :: Num a => a
width = 800

height :: Num a => a
height = 600

nextRandom :: (RandomGen g) => (Int, g) -> (Int, g)
nextRandom (a, g) = random g

asteroids :: StdGen -> Signal (Bool, Bool, Bool, Bool) -> State -> SignalGen (Signal (IO ()))
asteroids randomGenerator directionKey glossState = do
  randomSeries <- stateful (random randomGenerator) nextRandom
  let randomNumbers = fmap (\(a,b) -> a) randomSeries
  player <- playerSignal directionKey
  asteroids <- replicateM 8 $ asteroidSignal randomNumbers
  return $ render glossState <$> player <*> (sequence asteroids) <*> randomNumbers

playerSignal :: Signal (Bool, Bool, Bool, Bool) -> SignalGen (Signal Player)
playerSignal directionKey = do
  let Player initialPosition initialVelocity initialRotation = initialPlayer
  playerRotation <- transfer initialRotation updatePlayerRotation directionKey
  playerVelocity <- transfer2 initialVelocity updatePlayerVelocity directionKey playerRotation
  playerPosition <- transfer initialPosition updatePosition playerVelocity
  return $ Player <$> playerPosition <*> playerVelocity <*> playerRotation

updatePlayerRotation (l, r, _, _) rot
  | l && r = rot
  | l = rot - playerRotSpeed
  | r = rot + playerRotSpeed
  | not l && not r = rot

updatePlayerVelocity (_, _, u, d) rot (vx, vy)
  | u && d = (vx, vy)
  | u = (vx + playerAcceleration * xspeed, vy + playerAcceleration * yspeed)
  | d = (vx - playerAcceleration * xspeed, vy - playerAcceleration * yspeed)
  | not u && not d = (vx, vy)
  where
    (xspeed, yspeed) = angleToVector rot

updatePosition (vx, vy) (x, y) = (wrapW $ x+vx, wrapH $ y+vy)
  where
    halfWidth = width / 2
    halfHeight = height / 2
    wrap min max val = ((val - min) `mod'` (max-min)) + min
    wrapW = wrap (-halfWidth) (halfWidth)
    wrapH = wrap (-halfHeight) (halfHeight)

randomAsteroid = do
  --let path = map angleToVector [0,70..359]
  let allowNegative = subtract 1.0 . (*2.0)
  let halfWidth = width / 2
  let halfHeight = height / 2
  size <- getStdRandom random :: IO Float
  x <- getStdRandom random :: IO Float
  y <- getStdRandom random :: IO Float
  vx <- getStdRandom random :: IO Float
  vy <- getStdRandom random :: IO Float
  rot <- getStdRandom random :: IO Float
  angularVel <- getStdRandom random :: IO Float
  vertexCount <- getStdRandom (randomR (4,7)) :: IO Int
  vertexAngles <- replicateM vertexCount $ (getStdRandom (randomR (0.0, 360.0)) :: IO Float)
  let path = map angleToVector $ sort vertexAngles
  return $ Asteroid
            path
            (rangeAsteroidSize * size + minAsteroidSize)
            ( allowNegative (x * halfWidth)
            , allowNegative (y * halfHeight))
            ( rangeAsteroidVel* (allowNegative vx) + minAsteroidVel
            , rangeAsteroidVel* (allowNegative vy) + minAsteroidVel)
            rot
            (angularVel * maxAsteroidAngularVel)

randomAsteroids count = do
  asteroids <- replicateM count asteroidSignal
  return asteroids

asteroidSignal :: Signal Int -> SignalGen (Signal Asteroid)
asteroidSignal randomSignal = do
  Asteroid path size pos vel rot angularVel <- execute randomAsteroid
  asteroidSize <- snapshot $ fmap (fromIntegral . (+10) . flip mod' 10) randomSignal
  asteroidVel <- stateful vel id
  asteroidPos <- transfer pos updatePosition asteroidVel
  asteroidAngularVel <- stateful angularVel id
  asteroidRot <- transfer rot (\vel rot -> vel + rot) asteroidAngularVel
  return $ Asteroid path <$> asteroidSize <*> asteroidPos <*> asteroidVel <*> asteroidRot <*> asteroidAngularVel

-- Don't know how to generalise T_T
--randomSignal :: RandomGen g => g -> SignalGen (Signal a)
--randomSignal g = do
--  randomNumber <- stateful (random g) nextRandom
--  return $ fmap (\(a, b) -> a) randomNumber

main :: IO ()
main = do
  (directionKey, directionKeySink) <- external (False, False, False, False)
  glossState <- initState
  randomGenerator <- newStdGen
  withWindow width height "Hasteroids" $ \window -> do
    network <- start $ asteroids randomGenerator directionKey glossState
    let loop =
          do
            pollEvents
            readInput window directionKeySink
            join network
            swapBuffers window
            threadDelay timeStep
            esc <- keyIsPressed window Key'Escape
            unless esc loop
          in loop

render glossState player asteroids a = do
  displayPicture (width, height) black glossState 1.0 $
    Pictures $  [ renderPlayer player]
                ++ (map renderAsteroid asteroids)

renderPlayer (Player (x, y) _ rot) =
  Color white $
  translate x y $
  rotate rot $
  scale playerSize playerSize $
  lineLoop [(-0.707,-0.707), (0, 1), (0.707, -0.707)]

renderAsteroid (Asteroid path size (x, y) _ rot _) =
  Color green $
  translate x y $
  rotate rot $
  scale size size $
  lineLoop path

readInput window directionKeySink = do
  l <- keyIsPressed window Key'Left
  r <- keyIsPressed window Key'Right
  u <- keyIsPressed window Key'Up
  d <- keyIsPressed window Key'Down
  directionKeySink (l, r, u, d)

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.destroyWindow win
      Nothing -> return ()
    -- causes runtime error in ghci
    -- *** Exception: <stdout>: hPutChar: invalid argument (Bad file descriptor)
    -- GLFW.terminate
  where
    simpleErrorCallback e s =
      putStrLn $ unwords [show e, show s]

angleToVector rot = (sin (degToRad rot), cos (degToRad rot))
