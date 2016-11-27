module Main exposing (..)

import Html exposing (Html, program, div)
import Html.Attributes as HtmlA
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse exposing (Position)
import Window
import Task
import Random
import Voronoi exposing (voronoi)


main : Program Never Model Msg               
main = program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }



-- MODEL       


type alias Model =
    { position : Position
    , winSize : (Int, Int)
    , points : List (Int, Int)
    , polygons : List (List (Int, Int))
    , colors : List (Int, Int, Int)
    }


init : (Model, Cmd Msg)
init = (Model (Position 0 0) (0, 0) [] [] []
       , Task.perform Size Window.size)



-- SUBSCRIPTIONS    


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Mouse.clicks Click
              , Window.resizes Resize ]



-- UPDATE


type Msg = Click Position
         | Size Window.Size
         | Resize Window.Size
         | Points (List (Int, Int))
         | Colors (List (Int, Int, Int))
         | Color (Int, Int, Int)
        

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Points ps ->
            ({ model | points = ps
             , polygons = voronoi ps model.winSize }
            , Random.generate Colors (Random.list (List.length ps) randomColor))
        Size size ->
            ({model | winSize = (size.width, size.height)},
                 Random.generate Points
                 <| randomPoints 50 (size.width, size.height))
        Resize size ->
            ({ model | winSize = (size.width, size.height) }, Cmd.none)
        Color c ->
            ({ model | colors = c :: model.colors}, Cmd.none)
        Colors cs ->
            ({ model | colors = cs }, Cmd.none)
        Click pos ->
            let points_ = (pos.x, pos.y) :: model.points
            in ({ model | points = points_
                , polygons = voronoi points_ model.winSize}
               , Random.generate Color randomColor)



randomPoints : Int -> (Int, Int) -> Random.Generator (List (Int, Int))
randomPoints n (w, h) =
    Random.list n (Random.pair (Random.int 0 w) (Random.int 0 h))


randomColor : Random.Generator (Int, Int, Int)
randomColor =
    Random.map3 (,,) (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)    



-- VIEW        


svgCircle : (Int, Int) -> Svg msg
svgCircle (x, y) =
    circle [ fill "none"
           , stroke "black"
           , cx (toString x)
           , cy (toString y)
           , r "2"] []


svgPolygon : List (Int, Int) -> (Int, Int, Int) -> Svg msg             
svgPolygon ps (r, g, b) =
    let point (x,y) s = s ++ (toString x) ++ "," ++ (toString y) ++ " "
        rgb = "rgb(" ++ (toString r)
              ++ "," ++ (toString g)
              ++ "," ++ (toString b)++ ")"
    in polygon [ stroke "black" 
               , strokeWidth "2"
               , fill rgb
               , points <| List.foldr point "" ps
               ] []
        

view : Model -> Html Msg
view model =
    let (winW, winH) = model.winSize
        w = toString winW
        h = toString winH
    in div
        [ HtmlA.style [ ("padding", "0")
                      , ("margin", "0")
                      , ("display", "flex")
                      ]
        ]
        [ svg [ viewBox ("0 0 " ++ w ++ " " ++ h)
              , height "100%"
              , width "100%" 
              ]
              ((List.foldr (\(p, c) ps -> (svgPolygon p c) :: ps) []
                   <| List.map2 (,) model.polygons model.colors) ++
              (List.foldr (\p ps -> svgCircle p :: ps) [] model.points))
        ]
