{-# LANGUAGE RecursiveDo #-}

import Prelude hiding (until)
import Control.Applicative (liftA2)
import Control.Monad (when, join, unless, replicateM)
import Control.Monad.Fix (fix)
import Control.Monad.State
import Control.Concurrent (threadDelay)
import Graphics.UI.GLFW as GLFW
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Rendering as Gloss
import Graphics.Gloss.Geometry.Angle (degToRad)
import FRP.Elerea.Simple
import System.Exit
import System.Random
import Data.Fixed (mod')
import Data.List (sort)

import Random
import Window
import Maths

-- Game Types
type Point = Gloss.Point
type Path = Gloss.Path
type Pos = Point
type Vel = (Float, Float)
type AngularVel = Float
type Rot = Float
type Size = Float

data Player = Player Pos Vel Rot Bool

data Asteroid = Asteroid Path Size Pos Vel Rot AngularVel

data Bullet = Bullet Pos Vel

-- Initial game states
initialPlayer = Player (0, 0) (0, 0) 0 False

-- Game data
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

bulletSpeed = 5

shootTime :: Fractional a => a
shootTime = 1/8 -- seconds

-- Time each step
updateFreq :: Int
updateFreq = 60

timeStep :: Int
timeStep = 1000000 `quot` updateFreq

-- Window width and height
width, height :: Num a => a
width = 800
height = 600

-- Seconds to steps
steps :: Num a => a -> a
steps seconds = seconds * (fromIntegral updateFreq)

-- Update the shoot timer signal, current value -> whether shoot is pressed -> new value
updateShootTimer :: (Num a) => Bool -> a -> a
updateShootTimer shootPressed prev = if shootPressed then 0 else prev + 1

-- The main signal for the game
asteroids :: Gloss.State -> Signal (Bool, Bool, Bool, Bool) -> Signal Bool -> StateT StdGen SignalGen (Signal (IO ()))
asteroids glossState directionKey shootKey = mdo
  -- Player
  player <- playerSignal directionKey playerDead

  -- Initial asteroids
  asteroids <- replicateM 8 $ asteroidSignal

  -- Using a random asteroid for the initial value for delayed asteroid signals is dumb but easier
  randomAsteroid <- getRandomAsteroid

  -- Delayed signals
  player' <- lift $ delay initialPlayer player
  asteroids' <- lift $ sequence $ map (delay randomAsteroid) asteroids

  -- Map over asteroids to generate collision signals for the player with each asteroid
  playerAsteroidCollisions <- lift $ return $ fmap (playerAsteroidCollision <$> player' <*>) asteroids'

  -- Fold signals into one 'player colliding with asteroid' signal
  let playerColliding = foldS (||) False playerAsteroidCollisions

  -- Player dead signal
  playerDead <- lift $ transfer False (||) playerColliding
  playerAlive <- lift $ return $ not <$> playerDead

  lift $ mdo
    -- Shooting
    shootKeyPrev <- delay False shootKey
    shootPressed <- transfer2 False (\prev cur _ -> not prev && cur) shootKeyPrev shootKey
    shootTimer <- transfer (steps shootTime) updateShootTimer wasShooting
    canShoot <- return $ (>= (steps shootTime)) <$> shootTimer
    shooting <- return $ foldS (&&) True [canShoot, shootPressed, playerAlive]
    wasShooting <- delay False shooting
    newBullets <- generator (bulletGenerator <$> shooting <*> player)
    bullets <- collection newBullets ((\_ _ -> True) <$> shootPressed)

    return $ render glossState <$> player <*> (sequence asteroids) <*> bullets
    --return $ print <$> shooting

-- The player signal
playerSignal :: Signal (Bool, Bool, Bool, Bool) -> Signal Bool -> StateT StdGen SignalGen (Signal Player)
playerSignal directionKey colliding = do
  lift $ do
    let Player initialPosition initialVelocity initialRotation _ = initialPlayer
    playerRotation <- transfer initialRotation updatePlayerRotation directionKey
    playerVelocity <- transfer2 initialVelocity updatePlayerVelocity directionKey playerRotation
    playerPosition <- transfer initialPosition updatePosition playerVelocity
    return $ Player <$> playerPosition <*> playerVelocity <*> playerRotation <*> colliding

--playerCollisionSignal playerSignal asteroidSignal = playerAsteroidCollision <$> playerSignal <*> asteroidSignal

playerAsteroidCollision (Player (px, py) _ prot _) (Asteroid path size (ax, ay) _ _ _) =
      (ax - px)^2 + (ay - py)^2 < (size+playerSize)^2

-- Update the player rotation given the input direcitons
updatePlayerRotation (l, r, _, _) rot
  | l && r = rot
  | l = rot - playerRotSpeed
  | r = rot + playerRotSpeed
  | not l && not r = rot

-- Update the player velocity given the input directions
updatePlayerVelocity (_, _, u, d) rot (vx, vy)
  | u && d = (vx, vy)
  | u = (vx + playerAcceleration * xspeed, vy + playerAcceleration * yspeed)
  | d = (vx - playerAcceleration * xspeed, vy - playerAcceleration * yspeed)
  | not u && not d = (vx, vy)
  where
    (xspeed, yspeed) = angleToVector rot

-- Update a position given a velocity
updatePosition (vx, vy) (x, y) = (wrapW $ x+vx, wrapH $ y+vy)
  where
    halfWidth = width / 2
    halfHeight = height / 2
    wrap min max val = ((val - min) `mod'` (max-min)) + min
    wrapW = wrap (-halfWidth) (halfWidth)
    wrapH = wrap (-halfHeight) (halfHeight)

-- Get a random polygon of approximately unit size around (0, 0)
-- with a number of vertices between minVerts and maxVerts
getRandomPolygon :: Monad m => (Int, Int) -> StateT StdGen m [Point]
getRandomPolygon (minVerts, maxVerts)
  | minVerts < 3         = error "getRandomPolygon: minVerts must be greater than 2"
  | maxVerts <= minVerts = error "getRandomPolygon: maxVerts must be greater than minVerts"
  | otherwise            = do
    vertexCount <- getRandomR (minVerts, maxVerts)
    vertexAngles <- replicateM vertexCount $ getRandomR (0.0, 360.0)
    return $ map angleToVector $ sort vertexAngles

-- Get a randomly generated asteroid
getRandomAsteroid :: Monad m => StateT StdGen m Asteroid
getRandomAsteroid = Asteroid
                      <$> getRandomPolygon (4, 6)
                      <*> getRandomR (10.0, 25.0)
                      <*> getRandomPairR2 (-width/2, width/2) (-height/2, height/2)
                      <*> getRandomPairR1 (-1, 1)
                      <*> getRandomR (0.0, 360.0)
                      <*> getRandomR (0.0, 1.0)

-- The signal for a randomly generated asteroid
asteroidSignal :: StateT StdGen SignalGen (Signal Asteroid)
asteroidSignal = do
  Asteroid path size pos vel rot angularVel <- getRandomAsteroid
  lift $ do
    asteroidSize <- stateful (size :: Float) id
    asteroidVel <- stateful vel id
    asteroidPos <- transfer pos updatePosition asteroidVel
    asteroidAngularVel <- stateful angularVel id
    asteroidRot <- transfer rot (\vel rot -> vel + rot) asteroidAngularVel
    return $ Asteroid path <$> asteroidSize <*> asteroidPos <*> asteroidVel <*> asteroidRot <*> asteroidAngularVel

-- The signal for a new bullet with the specified position and velocity
bulletSignal :: Pos -> Vel -> SignalGen (Signal Bullet)
bulletSignal pos vel = do
  --lift $ do
    bulletVel <- stateful vel id
    bulletPos <- transfer pos updatePosition bulletVel
    return $ Bullet <$> bulletPos <*> bulletVel

-- Bullet generator
bulletGenerator :: Bool -> Player -> SignalGen [Signal Bullet]
bulletGenerator shoot (Player (px, py) (pvx, pvy) (prot) _) = do
  let playerDir@(playerDirX, playerDirY) = angleToVector prot
    in if shoot
          then (:[]) <$> bulletSignal
                          (px + playerDirX * playerSize, py + playerDirY * playerSize)
                          (playerDirX * bulletSpeed + pvx, playerDirY * bulletSpeed + pvy)
          else return []

-- Signal collector for bullets
collection :: Signal [Signal a] -> Signal (a -> Bool) -> SignalGen (Signal [a])
collection source isAlive = mdo
  boltSignals <- delay [] (map snd <$> boltsAndSignals')
  bolts <- memo (liftA2 (++) source boltSignals)
  let boltsAndSignals = zip <$> (sequence =<< bolts) <*> bolts
  boltsAndSignals' <- memo (filter <$>((.fst) <$> isAlive) <*> boltsAndSignals)
  return $ map fst <$> boltsAndSignals'

-- Entry point
main :: IO ()
main = do
  (directionKey, directionKeySink) <- external (False, False, False, False)
  (shootKey, shootKeySink) <- external False
  glossState <- Gloss.initState
  randomGenerator <- newStdGen
  withWindow width height "Hasteroids" $ \window -> do
    let game = asteroids glossState directionKey shootKey
    network <- start $ evalStateT game randomGenerator
    let loop =
          do
            pollEvents
            readInput window directionKeySink shootKeySink
            join network
            swapBuffers window
            threadDelay timeStep
            esc <- keyIsPressed window Key'Escape
            unless esc loop
          in loop

-- Render the game
render glossState player asteroids bullets = do
  Gloss.displayPicture (width, height) Gloss.black glossState 1.0 $
    Gloss.Pictures $  [ renderPlayer player]
                   ++ (map renderAsteroid asteroids)
                   ++ (map renderBullet bullets)

-- Render the player
renderPlayer (Player (x, y) _ rot dead) =
  if dead
    then
      Gloss.Blank
    else
      Gloss.Color Gloss.white $
      Gloss.translate x y $
      Gloss.rotate rot $
      Gloss.scale playerSize playerSize $
      Gloss.lineLoop [(-0.707,-0.707), (0, 1), (0.707, -0.707)]
      --where
      --  color
      --    | not colliding = Gloss.white
      --    |     colliding = Gloss.red

-- Render an asteroid
renderAsteroid (Asteroid path size (x, y) _ rot _) =
  Gloss.Color Gloss.green $
  Gloss.translate x y $
  Gloss.rotate rot $
  Gloss.scale size size $
  Gloss.lineLoop path

-- Render a bullet
renderBullet (Bullet (x, y) _) =
  Gloss.Color Gloss.white $
  Gloss.translate x y $
  Gloss.circle 3


-- Read inputs
readInput window directionKeySink shootKeySink = do
  l <- keyIsPressed window Key'Left
  r <- keyIsPressed window Key'Right
  u <- keyIsPressed window Key'Up
  d <- keyIsPressed window Key'Down
  s <- keyIsPressed window Key'Space
  directionKeySink (l, r, u, d)
  shootKeySink s

-- Fold over a signal
foldS :: (a -> b -> b) -> b -> [Signal a] -> Signal b
foldS f b signals = foldr (\signal acc -> f <$> signal <*> acc) (return b) signals
