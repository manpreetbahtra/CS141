--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Getting started                                                       --
--------------------------------------------------------------------------------

module Hatch (
    module Layout,
    module Transforms,
    module Images,
    runAnimation
) where

--------------------------------------------------------------------------------

import Graphics.Gloss
import Layout
import Transforms
import Images

--------------------------------------------------------------------------------

fps :: Int
fps = 30

runAnimation :: (Int -> Image) -> IO ()
runAnimation = animSteps fps

window :: Display
window = InWindow "CS141 Functional Programming" (1280, 960) (10, 10) --(1280, 960) 

background :: Color
background = makeColor 0.357 0.188 0.412 1
--background = white

animSteps :: Int -> (Int -> Image) -> IO ()
animSteps steps animFunc = animate window background (render . layout . animFunc . tStep)
    where
        tStep :: Float -> Int
        tStep t = ceiling (t/framePeriod)
        framePeriod = 1.0 / fromIntegral steps

--------------------------------------------------------------------------------
