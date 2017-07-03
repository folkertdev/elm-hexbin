module Example exposing (..)

{-| an example of using HexBins to plot data

Uses

* elm-community/typed-svg for the svg
* mgold/elm-random-pcg and kress95/random-pcg-extra for generating data,
* gampleman/elm-visualization for axes
-}

import HexBin exposing (HexBin)
import TypedSvg as Svg
import TypedSvg.Types as Svg exposing (Transform(Translate), Length(Px), ClipPath(ClipPathFunc))
import TypedSvg.Attributes exposing (..)
import TypedSvg.Attributes.InPx as InPx
import Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (id)
import Random.Pcg as Random
import Random.Pcg.Float as Random
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


{-| Generate some testing data (a normal distribution)
-}
generateData : Random.Generator (List ( Float, Float ))
generateData =
    Random.map2 (,) (Random.normal (config.width / 4) config.stdDev) (Random.normal (config.height / 4) config.stdDev)
        |> Random.list 2000


type Mode
    = ColorEncoding
    | AreaEncoding


type alias Model =
    { hexbin : HexBin.HexBin Int
    , mode : Mode
    }


type Msg
    = Receive (List ( Float, Float ))
    | Switch


init : ( Model, Cmd Msg )
init =
    let
        initial =
            HexBin.empty config.radius ( ( 0, 0 ), ( config.width, config.height ) ) 0
    in
        ( { hexbin = initial, mode = ColorEncoding }, Random.generate Receive generateData )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Receive items ->
            ( { model | hexbin = HexBin.updateMany (\v -> v + 1) items model.hexbin }
            , Cmd.none
            )

        Switch ->
            ( { model
                | mode =
                    if model.mode == ColorEncoding then
                        AreaEncoding
                    else
                        ColorEncoding
              }
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    let
        scaleX =
            Scale.linear ( 0, config.width ) ( 0, config.width )

        scaleY =
            Scale.linear ( 0, config.height ) ( config.height, 0 )

        xAxis =
            Svg.g [ class [ "axis" ], transform [ Translate 0 config.height ] ]
                [ Axis.axis { defaultOptions | orientation = Axis.Bottom } scaleX ]

        yAxis =
            Svg.g [ class [ "axis" ] ]
                [ Axis.axis { defaultOptions | orientation = Axis.Left } scaleY ]

        -- construct a clip path to hide hexagons outside/on the border of our plot
        clip =
            Svg.clipPath [ id "clip" ] [ Svg.rect [ InPx.width config.width, InPx.height config.height ] [] ]

        encoding =
            case model.mode of
                AreaEncoding ->
                    -- map the value (it is assumend that lies between 0 and 50) to a radius between 0 and 20.
                    HexBin.areaEncoding ( 0, 50 ) ( 0, 20 )

                ColorEncoding ->
                    -- map the value (it is assumed to be at most 20) to a color
                    -- the actual maximum value is larger than 20,
                    -- the use of LAB interpolation causes the color to become deeper/darker for those larger value
                    HexBin.colorEncoding 20

        hexagons =
            HexBin.render encoding model.hexbin
                |> Svg.g [ class [ "hexagon" ], clipPath (ClipPathFunc "url(#clip)") ]
    in
        Html.div []
            [ Svg.svg
                [ InPx.width (config.width + margin.left + margin.right)
                , InPx.height (config.height + margin.top + margin.bottom)
                ]
                [ Svg.g [ transform [ Translate margin.left margin.top ] ]
                    [ xAxis
                    , yAxis
                    , clip
                    , hexagons
                    ]
                ]
            , Html.button [ onClick Switch ] [ Html.text "Swich encoding" ]
            ]
