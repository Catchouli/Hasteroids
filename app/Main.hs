{-# LANGUAGE RecursiveDo #-}

import Control.Monad (when, join, unless)
import Control.Monad.Fix (fix)
import Control.Concurrent (threadDelay)
import Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering
import Graphics.Gloss.Geometry.Angle
import FRP.Elerea.Simple
import System.Exit
import System.Random
import Data.Fixed

type Pos = Point
type Vel = (Float, Float)
type Rot = Float
data Player = Player Pos Vel Rot

initialPlayer = Player (0, 0) (0, 0) 0

playerAcceleration = 0.5
playerRotSpeed = 10

width :: Num a => a
width = 800

height :: Num a => a
height = 600

asteroids :: Signal (Bool, Bool, Bool, Bool) -> State -> SignalGen (Signal (IO ()))
asteroids directionKey glossState = mdo
  player <- transfer initialPlayer updatePlayer directionKey
  return $ render glossState <$> player

updatePlayer :: (Bool, Bool, Bool, Bool) -> Player -> Player
updatePlayer (l, r, u, d) (Player (x, y) (vx, vy) rot) =
  Player (wrapW $ x+vx, wrapH $ y+vy) newVelocity newRot
  where
    halfWidth = width / 2
    halfHeight = height / 2
    wrap min max val = ((val - min) `mod'` (max-min)) + min
    wrapW = wrap (-halfWidth) (halfWidth)
    wrapH = wrap (-halfHeight) (halfHeight)
    (xspeed, yspeed) = movementVector rot
    newRot
      | l && r = rot
      | l = rot - playerRotSpeed
      | r = rot + playerRotSpeed
      | not l && not r = rot
    newVelocity
      | u && d = (vx, vy)
      | u = (vx + playerAcceleration * xspeed, vy + playerAcceleration * yspeed)
      | d = (vx - playerAcceleration * xspeed, vy - playerAcceleration * yspeed)
      | not u && not d = (vx, vy)

movementVector rot = (sin (degToRad rot), cos (degToRad rot))

main :: IO ()
main = do
  (directionKey, directionKeySink) <- external (False, False, False, False)
  glossState <- initState
  withWindow width height "Hasteroids" $ \window -> do
    network <- start $ asteroids directionKey glossState
    let loop =
          do
            pollEvents
            readInput window directionKeySink
            join network
            --render glossState initialPlayer
            swapBuffers window
            threadDelay 20000
            esc <- keyIsPressed window Key'Escape
            unless esc loop
          in loop

render glossState player = do
  displayPicture (width, height) white glossState 1.0 $
    Pictures $ [ renderPlayer player
               ]

renderPlayer (Player (x, y) _ rot) =
  Color blue $
  translate x y $
  rotate rot $
  scale 50 50 $
  polygon [(-0.707,-0.707), (0, 1), (0.707, -0.707)]

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
