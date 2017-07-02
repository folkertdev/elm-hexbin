module AreaEncoding exposing (..)

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
    , modifyRadius : { radius : Float, count : Int, row : Int, column : Int } -> Float
    }


colorEncoding : Config
colorEncoding =
    { cutoff = 20
    , displayEmpty = False
    , borderColor = Color.black
    , borderWidth = 0.5
    , fillColorMin = Color.white
    , fillColorMax = Color.rgb 70 130 180
    , modifyRadius = \{ radius } -> radius
    }


areaEncoding : Config
areaEncoding =
    { cutoff = 20
    , displayEmpty = False
    , borderColor = Color.white
    , borderWidth = 0.5
    , fillColorMin = Color.rgb 70 130 180
    , fillColorMax = Color.rgb 70 130 180
    , modifyRadius = \{ count } -> convert 0.5 ( 0, 50 ) ( 0, 20 ) count
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

        toRadius =
            convert 0.5 ( 0, 50 ) ( 0, 20 )

        path =
            HexBin.hexagonOrigin (config.modifyRadius { radius = radius, count = count, row = row, column = column })
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


raise x exponent =
    if x < 0 then
        -(-x ^ exponent)
    else
        x ^ exponent


deinterpolate exponent a b x =
    let
        a_ =
            raise a exponent

        b_ =
            raise b exponent - a_
    in
        ((raise x exponent) - a_) / b_


reinterpolate exponent a b t =
    let
        a_ =
            raise a exponent

        b_ =
            raise b exponent - a_
    in
        raise (a + b * t) (1 / exponent)


convert exponent =
    \domain range -> bimap domain range (deinterpolate exponent) interpolateFloat


invert exponent =
    \domain range -> bimap range domain (deinterpolate exponent) (reinterpolate exponent)


bimap :
    ( comparable0, comparable0 )
    -> ( comparable1, comparable1 )
    -> (comparable0 -> comparable0 -> a -> b)
    -> (comparable1 -> comparable1 -> b -> c)
    -> (a -> c)
bimap ( d0, d1 ) ( r0, r1 ) deinterpolate reinterpolate =
    let
        ( de, re ) =
            if d1 < d0 then
                ( deinterpolate d1 d0, reinterpolate r1 r0 )
            else
                ( deinterpolate d0 d1, reinterpolate r0 r1 )
    in
        re << de


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat from to time =
    from + (to - from) * time
