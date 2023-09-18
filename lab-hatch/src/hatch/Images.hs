--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Getting started                                                       --
--------------------------------------------------------------------------------

module Images where

--------------------------------------------------------------------------------

import qualified Graphics.Gloss as Gloss 
    (blank, text, scale, 
        color, white, pictures
        , translate)
import Graphics.Gloss.Juicy ( loadJuicyPNG )
import System.IO.Unsafe ( unsafePerformIO )
import Layout
import Transforms

import Data.Maybe (fromMaybe)
import Graphics.Gloss (Picture)

--------------------------------------------------------------------------------

loadPNG :: String -> Image
loadPNG name = scale 0.1 $ Leaf $ png $ "assets/" ++ name ++ ".png"

png :: FilePath -> Picture
png fname = fromMaybe (Gloss.text "PNG ERROR")
            $ unsafePerformIO 
            $ loadJuicyPNG fname

cat :: Image
cat = loadPNG "cat"

dog :: Image
dog = loadPNG "dog"

duck :: Image
duck = loadPNG "duck"

goose :: Image
goose = loadPNG "goose"

blank :: Image
blank = Leaf Gloss.blank

text :: String -> Image
text t = Leaf $ Gloss.pictures [
            Gloss.translate x y 
                $ Gloss.color Gloss.white 
                $ Gloss.scale 0.5 0.5 
                $ Gloss.text t
            | x <- [-2..2]
            , y <- [-2..2]
        ]

--------------------------------------------------------------------------------