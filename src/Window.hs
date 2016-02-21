module Window
    ( withWindow
    , keyIsPressed
    ) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (when)

-- Whether a key is pressed
keyIsPressed :: GLFW.Window -> GLFW.Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

-- Whether a keystate is a key press
isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

-- Run an io function with a window
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
