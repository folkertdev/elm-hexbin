module Example exposing (..)

import TypedSvg as Svg
import TypedSvg.Types as Svg exposing (Transform(Translate), Length(Px), ClipPath(ClipPathFunc))
import TypedSvg.Attributes exposing (..)
import TypedSvg.Attributes.InPx as InPx
import Random.Pcg as Random
import Random.Pcg.Float as Random
import Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (id)
import HexBin exposing (HexBin)
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


generateData =
    Random.map2 (,) (Random.normal (config.width / 4) config.stdDev) (Random.normal (config.height / 4) config.stdDev)
        |> Random.list 2000


view : Model -> Html.Html Msg
view model =
    let
        scaleX =
            Scale.linear ( 0, config.width ) ( 0, config.width )

        scaleY =
            Scale.linear ( 0, config.height ) ( config.height, 0 )

        xAxis =
            Svg.g [ class [ "axis" ], transform [ uncurry Translate ( 0, config.height ) ] ]
                [ Axis.axis { defaultOptions | orientation = Axis.Bottom, tickFormat = Just (Scale.tickFormat scaleX 2) } scaleX ]

        yAxis =
            Svg.g [ class [ "axis" ], transform [ uncurry Translate ( 0, 0 ) ] ]
                [ Axis.axis { defaultOptions | orientation = Axis.Left } scaleY ]

        clip =
            Svg.clipPath [ id "clip" ] [ Svg.rect [ InPx.width config.width, InPx.height config.height ] [] ]

        hexagons =
            case model.mode of
                AreaEncoding ->
                    HexBin.render (HexBin.areaEncoding ( 0, 50 ) ( 0, 20 )) model.hexbin
                        |> Svg.g [ class [ "hexagon" ], clipPath (ClipPathFunc "url(#clip)") ]

                ColorEncoding ->
                    HexBin.render (HexBin.colorEncoding 20) model.hexbin
                        |> Svg.g [ class [ "hexagon" ], clipPath (ClipPathFunc "url(#clip)") ]
    in
        Html.div []
            [ Svg.svg
                [ InPx.width (config.width + margin.left + margin.right)
                , InPx.height (config.height + margin.top + margin.bottom)
                ]
                [ Svg.g [ transform [ uncurry Translate ( margin.left, margin.top ) ] ]
                    [ xAxis
                    , yAxis
                    , clip
                    , hexagons
                    ]
                ]
            , Html.button [ onClick Switch ] [ Html.text "Swich encoding" ]
            ]
