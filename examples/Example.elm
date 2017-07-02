module Example exposing (..)

import Svg
import Svg.Attributes exposing (..)
import Random.Pcg as Random
import Random.Pcg.Float as Random
import Html
import Html.Events exposing (onClick)
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
            Svg.g [ class "axis", transform (translate ( 0, config.height )) ]
                [ Axis.axis { defaultOptions | orientation = Axis.Bottom, tickFormat = Just (Scale.tickFormat scaleX 2) } scaleX ]

        yAxis =
            Svg.g [ class "axis", transform (translate ( 0, 0 )) ]
                [ Axis.axis { defaultOptions | orientation = Axis.Left } scaleY ]

        clip =
            Svg.clipPath [ id "clip" ] [ Svg.rect [ width (toString config.width), height (toString config.height) ] [] ]

        hexagons =
            case model.mode of
                AreaEncoding ->
                    HexBin.render (HexBin.areaEncoding ( 0, 50 ) ( 0, 20 )) model.hexbin
                        |> Svg.g [ class "hexagon", clipPath "url(#clip)" ]

                ColorEncoding ->
                    HexBin.render (HexBin.colorEncoding 20) model.hexbin
                        |> Svg.g [ class "hexagon", clipPath "url(#clip)" ]
    in
        Html.div []
            [ Svg.svg
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
            , Html.button [ onClick Switch ] [ Html.text "Swich encoding" ]
            ]


translate : ( number1, number2 ) -> String
translate ( x, y ) =
    "translate(" ++ Basics.toString x ++ "," ++ Basics.toString y ++ ")"
