module Example exposing (..)

import Svg
import Svg.Attributes exposing (..)
import Random.Pcg as Random
import Random.Pcg.Float as Random
import Html
import Color
import HexBin exposing (HexBin)
import ColorEncoding
import AreaEncoding
import Visualization.Scale as Scale
import Visualization.Axis as Axis exposing (defaultOptions)


main : Program Never Model Msg
main =
    Html.program
        { update = update
        , view = view
        , init = init
        , subscriptions = \_ -> Sub.none
        }


margin =
    { top = 20, right = 20, bottom = 30, left = 40 }


config =
    { width = 960, height = 500, radius = 20, stdDev = 50 }


type alias Model =
    HexBin.HexBin Int


type Msg
    = Receive (List ( Float, Float ))


init : ( Model, Cmd Msg )
init =
    let
        initial : Model
        initial =
            HexBin.empty config.radius ( ( 0, 0 ), ( config.width, config.height ) ) 0
    in
        ( initial, Random.generate Receive generateData )


update : Msg -> Model -> ( Model, Cmd Msg )
update (Receive items) model =
    ( HexBin.insertManyWith (\v -> v + 1) items model
    , Cmd.none
    )


generateData =
    Random.map2 (,) (Random.normal (config.width / 4) config.stdDev) (Random.normal (config.height / 4) config.stdDev)
        |> Random.list 2000


view : Model -> Html.Html msg
view model =
    let
        scaleX =
            Scale.linear ( 0, config.width ) ( 0, config.width )

        scaleY =
            Scale.linear ( 0, config.height ) ( config.height, 0 )

        xAxis =
            Svg.g [ class "axis", transform (translate ( 0, config.height )) ]
                [ Axis.axis { defaultOptions | orientation = Axis.Bottom, tickFormat = Just (Scale.tickFormat scaleX 2) } scaleX ]

        yAxis =
            Svg.g [ class "axis", transform (translate ( 0, 0 )) ]
                [ Axis.axis { defaultOptions | orientation = Axis.Left } scaleY ]

        clip =
            Svg.clipPath [ id "clip" ] [ Svg.rect [ width (toString config.width), height (toString config.height) ] [] ]

        hexagons =
            AreaEncoding.hexagons AreaEncoding.areaEncoding model
                |> Svg.g [ class "hexagon", clipPath "url(#clip)" ]
    in
        Svg.svg
            [ width (toString <| config.width + margin.left + margin.right)
            , height (toString <| config.height + margin.top + margin.bottom)
            ]
            [ Svg.g [ transform (translate ( margin.left, margin.top )) ]
                [ xAxis
                , yAxis
                , clip
                , hexagons
                ]
            ]


translate : ( number1, number2 ) -> String
translate ( x, y ) =
    "translate(" ++ Basics.toString x ++ "," ++ Basics.toString y ++ ")"
