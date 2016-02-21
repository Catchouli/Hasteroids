module Maths
    ( angleToVector
    ) where

import Graphics.Gloss.Geometry.Angle (degToRad)

angleToVector rot = (sin (degToRad rot), cos (degToRad rot))
