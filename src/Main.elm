import Html exposing (..)
import Html.App as App
import Time exposing (Time)
import AnimationFrame exposing (diffs)
import Collage exposing (..)
import Element exposing (Element, toHtml)
import Color exposing (Color, rgb)
import Window exposing (Size, resizes)
import Random exposing (Generator, int, float)
import Task

main = App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }



-- MODEL

type alias Position =
  ( Float
  , Float
  )

type alias Velocity =
  ( Float
  , Float
  )

type alias Boid =
  { pos : Position
  , vel : Velocity
  , color : Color
  }

boid : Boid
boid =
  { pos = (20, 20)
  , vel = (1, 1)
  , color = (rgb 44 234 163)
  }

type alias Model =
  { time : Time
  , windowSize : Size
  , boids : List Boid
  }



-- RANDOM GENERATORS


randomColor : Generator (Color)
randomColor =
  Random.map3 rgb (int 0 255) (int 0 255) (int 0 255)


randomTuple : Float -> Float -> Generator (Float, Float)
randomTuple min max =
  Random.pair (float min max) (float min max)


randomPosition : Generator Position
randomPosition =
  randomTuple -1000 1000


randomVelocity : Generator Velocity
randomVelocity =
  randomTuple -5 5


randomBoid : Generator Boid
randomBoid =
  Random.map3 Boid
    randomPosition
    randomVelocity
    randomColor


randomBoids : Generator (List Boid)
randomBoids =
  Random.list 100 randomBoid



-- INIT

init : (Model, Cmd Msg)
init =
  ( Model 0 (Size 1 1) []
  , Cmd.batch
    [ Task.perform (\_ -> NoOp) Resize Window.size
    , Random.generate InitBoids randomBoids
    ]
  )


-- UPDATE

type Msg
  = Tick Time
  | Resize Size
  | InitBoids (List Boid)
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model
        | time = newTime
        , boids = tickBoids model.boids
        }
      , Cmd.none
      )

    Resize size ->
      ( { model | windowSize = size }
      , Cmd.none
      )

    InitBoids boids ->
      ( { model | boids = boids }
      , Cmd.none
      )

    NoOp ->
      ( model
      , Cmd.none
      )


tickBoids : (List Boid) -> (List Boid)
tickBoids boids =
  List.map (tickBoid boids) boids


tickBoid : (List Boid) -> Boid -> Boid
tickBoid boids boid =
  let
    (x, y) = boid.pos
    (dx, dy) = boid.vel
    newPos = (x + dx, y + dy)
  in
    { boid | pos = newPos }



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ diffs Tick
  , resizes Resize
  ]



-- VIEW

view : Model -> Html Msg
view model =
  div [] [ toHtml (canvas model)]


canvas : Model -> Element
canvas model =
  collage
    model.windowSize.width
    model.windowSize.height
    (List.map (drawBoid model.windowSize) model.boids)


drawBoid : Size -> Boid -> Form
drawBoid windowSize boid =
  circle 3
  |> filled boid.color
  |> move (wrapPosition windowSize boid.pos)


wrapPosition : Size -> Position -> Position
wrapPosition {width, height} (x, y) =
  let
    maxX = (toFloat width) / 2
    maxY = (toFloat height) / 2

    minX = -1 * maxX
    minY = -1 * maxY

    newX = wrapFloat minX maxX x
    newY = wrapFloat minY maxY y
  in
    (newX, newY)


wrapFloat : Float -> Float -> Float -> Float
wrapFloat min max x =
  let
    range = max - min
  in
    if x > max then
      wrapFloat min max (x - range)
    else if x < min then
      wrapFloat min max (x + range)
    else
      x
