{-
  	Copyright 2014 Bryan Hoyle

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}
import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Mouse as Mouse
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Text as Text

data Object = Object { x :: Double, y :: Double,
                       vx :: Double, vy :: Double}

data GameState = GameState { ball :: Object,
                             lpaddle :: Object,
                             rpaddle :: Object,
                             lscore :: Int,
                             rscore :: Int }


applyAccel obj (ax,ay) dt = obj {vx = (ax * (Time.inSeconds dt)) + vx obj,
                                   vy = (ay * (Time.inSeconds dt)) + vy obj}

applyDampen obj factor = obj { vx = factor * (vx obj),
                               vy = factor * (vy obj) }

applyPosition obj (nx,ny) = obj {x = nx, y = ny}

applyVelocity dt obj = obj { x = (vx obj) * (Time.inSeconds dt) + x obj,
                             y = (vy obj) * (Time.inSeconds dt) + y obj }

accelByMouse obj factor (mx,my) = (ax,ay)
  where ax = 0*(mx - (x obj))/factor
        ay = (my - (y obj))/factor

renderPaddle pdl = move (x pdl,y pdl) $ filled red $ rect 10 30
renderBall ball = move (x ball, y ball) $ filled red $ circle 5

renderScore score =
  toForm $ Text.text $ Text.color red $ Text.header $ Text.toText $ show score

render :: (Int, Int) -> GameState -> Element
render (w,h) (GameState {ball = ball, lpaddle = lpaddle, rpaddle = rpaddle,
                         lscore = lscore, rscore = rscore}) =
  collage w h [renderBall ball,
               renderPaddle lpaddle,
               renderPaddle rpaddle,
               move (100,100) $ renderScore lscore,
               move ((fromIntegral w) - 100,100) $ renderScore rscore ]

bindLeft (x,y) obj =
  obj { x = 0 }

bindRight (x,y) obj =
  obj { x = (fromIntegral x) }


paddleAi pdl ball dt =
  applyDampen (applyAccel pdl (accelByMouse pdl 0.02 (x ball, y ball)) dt) 0.995

paddleHuman pdl (mx,my) =
  pdl { x = (fromIntegral mx),
        y = (fromIntegral my) }


inPaddle ball pdl =
  (abs ((x ball) - (x pdl))) < 5 &&
  (y ball) < (y pdl) + 15 &&
  (y ball) > (y pdl) - 15

ballAi :: GameState -> (Int,Int) -> GameState
ballAi gs (w,h) =
  if inPaddle (ball gs) (lpaddle gs) then
    gs {ball = (ball gs) {vx = -(vx (ball gs))*1.09,
                          vy = (vy (ball gs)) + (vy (lpaddle gs))}}
  else if inPaddle (ball gs) (rpaddle gs) then
    gs {ball = (ball gs) {vx = -(vx (ball gs))*1.09,
                          vy = (vy (ball gs)) + (vy (rpaddle gs))}}
  else if x (ball gs) <= 5 then
         gs {rscore = (rscore gs) + 1,
             ball = Object {x = 30, y = 30, vx = 200, vy = 200}}
  else if x (ball gs) >= (fromIntegral w) then
         gs {lscore = (lscore gs) + 1,
             ball = Object {x = 30, y = 30, vx = 200, vy = 200}}
  else if y (ball gs) >= (fromIntegral h)-5 || y (ball gs) <= 0 then
         gs { ball = (ball gs) {vy = -(vy (ball gs))}} 
  else
    gs

step ((mx,my),dt,dim) gs =
  ngs { rpaddle = applyVelocity dt (bindRight dim nrpaddle),
       lpaddle = applyVelocity dt (bindLeft dim nlpaddle),
       ball = applyVelocity dt nball }
  where
    nrpaddle = paddleHuman (rpaddle gs) (mx,my)
    nlpaddle = paddleAi (lpaddle gs) (ball gs) dt
    ngs = ballAi gs dim 
    nball = ball ngs
    
main :: IO ()
main = do
  run defaultConfig $ render <~ Window.dimensions ~~ stepper
  where
    ball = Object { x = 100, y = 100, vx = 200, vy = 200 }
    lp = Object { x = 0, y = 0, vx = 0, vy = 0 }
    rp = Object { x = 0, y = 0, vx = 0, vy = 0 }
    gs = GameState {ball = ball, lpaddle = lp, rpaddle = rp,lscore = 0, rscore = 0}
    stepper = foldp step gs $ lift3 (,,) Mouse.position Time.delta Window.dimensions
