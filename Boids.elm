import Html exposing (Html, div)
import Html.App as App
import AnimationFrame
import Collage exposing (..)
import Element exposing (Element, toHtml)
import Color exposing (Color, hsl)
import Window exposing (Size)
import Random exposing (Generator, int, float)
import Task



-- MAIN PROGRAM

main = App.program
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }



-- MODEL

type alias Position = Vector
type alias Velocity = Vector

type alias Boid =
  { pos : Position
  , vel : Velocity
  , color : Color
  }

type alias Model =
  { boids : List Boid
  , windowSize : Size
  }



-- INIT

init : (Model, Cmd Msg)
init =
  ( Model [] (Size 1 1)
  , Cmd.batch
    [ Random.generate InitBoids randomBoids
    , Task.perform (\_ -> NoOp) Resize Window.size
    ]
  )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ AnimationFrame.diffs (\_ -> Tick)
  , Window.resizes Resize
  ]



-- UPDATE

type Msg
  = Tick
  | InitBoids (List Boid)
  | Resize Size
  | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      { model | boids = tickBoids model }
      ! []

    Resize size ->
      { model | windowSize = size }
      ! []

    InitBoids boids ->
      { model | boids = boids }
      ! []

    NoOp ->
      model
      ! []


tickBoids : Model -> List Boid
tickBoids model =
  List.map
    (tickBoid model)
    model.boids


tickBoid : Model -> Boid -> Boid
tickBoid model boid =
  let
    v' =
      rules
      |> List.map (applyRule model boid)
      |> sum
      |> add boid.vel
      |> limitSpeed params.maxSpeed

    p' =
      v'
      |> add boid.pos
      |> wrapPosition model.windowSize
  in
    { boid
    | vel = v'
    , pos = p'
    }



-- RULES

type alias Rule = Model -> Boid -> Velocity


applyRule : Model -> Boid -> (Rule, Float, Float) -> Velocity
applyRule model boid (rule, weight, radius) =
  let
    ns =
      neighbours
        radius
        boid
        model.boids

    v =
      rule { model | boids = ns } boid
  in
    multiply weight v


rules : List (Rule, Float, Float)
rules =
  [ (   cohesion, 0.014, 100 )
  , (  alignment, 0.025,  20 )
  , ( separation, 1.000,  50 )
  ]


cohesion : Rule
cohesion {boids} b =
  let
    c =
      boids
      |> List.map .pos
      |> average
  in
    subtract c b.pos


alignment : Rule
alignment {boids} b =
  boids
  |> List.map .vel
  |> average


separation : Rule
separation {boids} b =
  let
    separate b' =
      let
        d = subtract b'.pos b.pos
        m = magnitude d
      in
        multiply (-20 / m ^ 2) d
  in
    boids
    |> List.map separate
    |> average



-- HELPERS

neighbours : Float -> Boid -> List Boid -> List Boid
neighbours radius b bs =
  bs
  |> List.filter ((/=) b)
  |> List.filter (\b' -> magnitude (subtract b'.pos b.pos) <= radius)


limitSpeed : Float -> Velocity -> Velocity
limitSpeed limit v =
  let
    m = min (magnitude v) limit
    d = direction v
  in
    polar2rect m d


wrapPosition : Size -> Position -> Position
wrapPosition { width, height } (x, y) =
  let
    maxX = (toFloat width) / 2
    maxY = (toFloat height) / 2
  in
    ( wrapFloat (-1 * maxX) maxX x
    , wrapFloat (-1 * maxY) maxY y
    )


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



-- VIEW

view : Model -> Html Msg
view model =
  div [] [ toHtml (canvas model) ]


canvas : Model -> Element
canvas { windowSize, boids } =
  collage
    windowSize.width
    windowSize.height
    (List.map (drawBoid windowSize) boids)


drawBoid : Size -> Boid -> Form
drawBoid windowSize boid =
  circle 3
  |> filled boid.color
  |> move boid.pos



-- VECTORS

type alias Vector = (Float, Float)

pairwise : (Float -> Float -> Float) -> Vector -> Vector -> Vector
pairwise op v1 v2 =
  ( op (fst v1) (fst v2)
  , op (snd v1) (snd v2)
  )


add : Vector -> Vector -> Vector
add = pairwise (+)


subtract : Vector -> Vector -> Vector
subtract = pairwise (-)


dot : Vector -> Vector -> Vector
dot = pairwise (*)


multiply : Float -> Vector -> Vector
multiply k = dot (k, k)


magnitude : Vector -> Float
magnitude v =
  sqrt ((fst v) ^ 2 + (snd v) ^ 2)


direction : Vector -> Float
direction v =
  atan2 (snd v) (fst v)


sum : (List Vector) -> Vector
sum vs =
  List.foldl add (0, 0) vs


average : (List Vector) -> Vector
average vs =
  let
    n = List.length vs
    s = sum vs
  in
    if n > 0 then
      multiply (1 / (toFloat n)) s
    else
      (0, 0)


polar2rect : Float -> Float -> Vector
polar2rect magnitude direction =
  ( magnitude * cos direction
  , magnitude * sin direction
  )



-- RANDOM GENERATORS

params =
  { nBoids = 70
  , maxSpeed = 5.0
  , maxPosition = 1000.0
  }


randomColor : Generator Color
randomColor =
  Random.map3
    hsl
    (Random.map degrees (float 0 360))
    (float 0.7 0.8)
    (float 0.7 0.8)



randomVector : Float -> Float -> Generator Vector
randomVector min max =
  Random.pair
    (float min max)
    (float min max)


randomPosition : Generator Position
randomPosition =
  randomVector
    (params.maxPosition * -1)
    params.maxPosition


randomVelocity : Generator Velocity
randomVelocity =
  randomVector
    (params.maxSpeed * -1)
    params.maxSpeed


randomBoid : Generator Boid
randomBoid =
  Random.map3 Boid
    randomPosition
    randomVelocity
    randomColor


randomBoids : Generator (List Boid)
randomBoids =
  Random.list params.nBoids randomBoid
