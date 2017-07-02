module ColorEncoding exposing (..)

import HexBin exposing (HexBin)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Color.Interpolate exposing (Space(..), interpolate)
import Svg.Path as Path exposing (subpath, startAt, lineToMany, closed)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


type alias Config =
    { cutoff : Float
    , displayEmpty : Bool
    , borderColor : Color
    , borderWidth : Float
    , fillColorMin : Color
    , fillColorMax : Color
    }


defaultConfig : Config
defaultConfig =
    { cutoff = 20
    , displayEmpty = False
    , borderColor = Color.black
    , borderWidth = 0.5
    , fillColorMin = Color.white
    , fillColorMax = Color.rgb 70 130 180
    }


hexagons : Config -> HexBin Int -> List (Svg msg)
hexagons config hexbin =
    hexbin
        |> HexBin.indexedMap (renderHexagon config (HexBin.radius hexbin))
        |> HexBin.toList


renderHexagon : Config -> Float -> Int -> Int -> Int -> Svg.Svg msg
renderHexagon config radius row column count =
    let
        t =
            HexBin.hexagonTranslate row column radius

        path =
            HexBin.hexagonOrigin radius
                |> polyline

        fillColor =
            interpolate LAB config.fillColorMin config.fillColorMax (toFloat count / config.cutoff)
                |> colorToHex
    in
        if config.displayEmpty || count > 0 then
            Svg.path
                [ d (Path.pathToString path)
                , transform (translate t)
                , fill fillColor
                , stroke (colorToHex config.borderColor)
                , strokeWidth (toString config.borderWidth)
                ]
                []
        else
            Svg.text ""


polyline : List ( Float, Float ) -> Path.Path
polyline points =
    case points of
        [] ->
            []

        x :: xs ->
            [ subpath (startAt x) closed [ lineToMany xs ] ]


translate : ( number1, number2 ) -> String
translate ( x, y ) =
    "translate(" ++ Basics.toString x ++ "," ++ Basics.toString y ++ ")"
