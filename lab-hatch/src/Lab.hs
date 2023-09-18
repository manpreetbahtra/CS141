--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Getting started                                                       --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

import Hatch

--------------------------------------------------------------------------------

-- animation t =     offset (-400)   200  (dogwheel t)
--               <@> offset (-400) (-200) (gooseChase t)
--               <@> offset  400    200   (dogwheel t)
--               <@> offset  0      200   (scale 6 duck <@> rotate (t) dog)
--               <@> offset (-300)  0     (text "Welcome to CS141!")

-- A load of dogs spinning around
-- dogwheel t = superimposeAll [spinningDog x | x <- [t, t + 45 .. t + 315 ] ]

-- One single rotating dog
-- spinningDog t = rotate t (offset 100 0 dog)

-- The bottom of the screen goose chase
-- gooseChase t = offset (t * 2) 0 (mirror goose <|> duck <|> duck)

--------------------------------------------------------------------------------

--ex5 creative
animation t =   offset (-440) 200 (duckwheel t)
            <@> offset (-440) (-180) (dogChase t)
            <@> offset 400 100 (duckwheel t)
            <@> offset  (-1)  259 (mirror goose <@> text "Nothing creative :(")

duckwheel t = superimposeAll [spinningDuck x | x <- [t, t + 90 .. t + 270 ] ]

spinningDuck t = rotate t (offset 0 100 duck)

dogChase t = offset (t * (-1)) 0 (mirror cat <|> dog <|> cat)
            
