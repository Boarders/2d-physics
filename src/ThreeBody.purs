module ThreeBody where

import Prelude

import Data.Maybe
import Effect
import Graphics.Canvas as C
import Signal as S
import Signal.DOM as S
import Partial.Unsafe (unsafePartial)
import Effect.Console
import Data.Monoid
import Math(pi, min)
import Web.HTML.Window (innerWidth, innerHeight)
import Web.HTML (window)
import Data.Int (toNumber)
import Data.Functor ((<$>))
import Signal.Time (millisecond, every)
import AD

main :: Effect Unit
main = unsafePartial $ do
  pure unit
  curr_window <- window
  width <- toNumber <$> innerWidth curr_window
  height <- toNumber <$> innerHeight curr_window
  let dim = {width : width, height : height }
  log "starting main"
  Just canvas <- C.getCanvasElementById "canvas"
  C.setCanvasWidth canvas width
  C.setCanvasHeight canvas height
--  dimensions <- C.getCanvasDimensions canvas
  log ("width " <> show width)
  log ("height " <> show height)  
  context <- C.getContext2D canvas
  let ticker = every (25.0 * millisecond)
  let game = S.foldp (const update) initialState ticker
--  S.runSignal (render dim context <$> game)
  render dim context initialState


update :: State -> State
update  state = state
{-  let
    curr_height = state.y_pos
    state' =
      if state.y_pos > height - state.radius
      then state {step = -state.step}
      else if state.y_pos < state.radius
      then state {step = -state.step}
      else state
  in
    state' { y_pos = state'.y_pos + state'.step }
  where
    mid_height = height / 2.0
-}

render :: Dimensions -> C.Context2D -> State -> Effect Unit
render dim context state = do
--   log ("y_pos" <> show state.y_pos)
  clearCanvas dim context
  renderSystem dim state.system context



clearCanvas dim context = do
  C.clearRect context { x: 0.0, y: 0.0, width: dim.width, height: dim.height }


type Body =
    { mass :: Number
    , x :: Number
    , y :: Number
    , v_x :: Number
    , v_y :: Number
    , radius :: Number
    }

body1 :: Body 
body1 = { mass : 10.0, x : 40.0, y : 40.0, v_x : -5.0, v_y : 1.0, radius : 5.0 }

body2 :: Body
body2 = { mass : 20.0, x : 60.0, y : 60.0, v_x : 4.0, v_y : -2.0, radius : 10.0 }

body3 :: Body
body3 = { mass : 30.0, x : 30.0, y : 60.0, v_x : 0.0, v_y : 0.0, radius : 15.0 }

type System
  = { body1 :: Body, body2 :: Body, body3 :: Body }


type State
  = { system :: System, step :: Number}

initialSystem :: System
initialSystem = { body1 : body1, body2 : body2, body3 : body3}

initialState :: State
initialState  =
  { system : initialSystem, step : 5.0 }

type Colour = String

red = "#D0312D"
green = "#249225"
blue = "#3844BC"

renderSystem :: Dimensions -> System -> C.Context2D -> Effect Unit
renderSystem dims sys ctxt = do
  let cb1 = relPosToCanvasPos dims sys.body1
  let cb2 = relPosToCanvasPos dims sys.body2
  let cb3 = relPosToCanvasPos dims sys.body3
  renderCanvasPos dims red   cb1 ctxt
  renderCanvasPos dims green cb2 ctxt
  renderCanvasPos dims blue  cb3 ctxt
  
  


renderCanvasPos :: Dimensions -> Colour -> CanvasPos -> C.Context2D -> Effect Unit
renderCanvasPos dim col pt context = do
  log ("x : " <> show pt.canvas_x)
  log ("y : " <> show pt.canvas_y)
  log ("col : " <> show col)   
  C.beginPath context
  C.setFillStyle context col-- "#D0312D"
  C.arc context
    { x : pt.canvas_x
    , y : pt.canvas_y
    , radius : pt.radius
    , start: 0.0
    , end: 2.0 * pi
    }
  C.fill context
  C.closePath context 




type CanvasPos = {canvas_x :: Number, canvas_y :: Number, radius :: Number}
type Dimensions = {width :: Number, height :: Number}

relPosToCanvasPos
  :: forall r . Dimensions -> {x :: Number, y :: Number, radius :: Number | r} -> CanvasPos
relPosToCanvasPos dim relPos =
  { canvas_x : dim.width * (relPos.x / 100.0)
  , canvas_y : dim.height - ((relPos.y / 100.0) * dim.height) 
  , radius : relPos.radius
  }
