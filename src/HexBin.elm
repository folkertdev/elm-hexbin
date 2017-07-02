module HexBin
    exposing
        ( HexBin
        , Config
        , empty
        , map
        , indexedMap
        , update
        , updateMany
        , get
        , set
        , modify
        , render
        , areaEncoding
        , colorEncoding
        , toMatrix
        , toList
        )

{-|

# HexBin

A module for binning 2D points into hexagons

## Data
@docs HexBin

## Construct
@docs empty

## Insert
@docs update, updateMany

## Render
@docs render, areaEncoding, colorEncoding, Config

## Modify
@docs map, indexedMap, get, set, modify

## Convert
@docs toList, toMatrix

-}

import Matrix exposing (Matrix)
import BoundingBox as BBox
import Vec2
import Array
import Color exposing (Color)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Color.Convert exposing (colorToHex)
import Color.Interpolate exposing (Space(..), interpolate)
import Svg.Path as Path exposing (subpath, startAt, lineToMany, closed)


{-| Opaque type representing a grid of hexagon-shaped cells/bins.
-}
type HexBin a
    = HexBin
        { cells : Matrix a
        , bbox : BBox.BoundingBox
        , radius : Float
        }


{-| Construct a new `HexBin`.

* `radius`: The radius of the hexagons in the grid
* `(bottomLeft, topRight)`: The extend of the plot. Data outside of it will be ignored.
* `v`: The initial value of every cell
-}
empty : Float -> ( ( Float, Float ), ( Float, Float ) ) -> a -> HexBin a
empty radius ( bottomLeft, topRight ) v =
    let
        bbox =
            BBox.fromCorners (Vec2.fromTuple bottomLeft) (Vec2.fromTuple topRight)

        width =
            ceiling (BBox.width bbox / radius)

        height =
            ceiling (BBox.height bbox / radius)
    in
        HexBin
            { cells = Matrix.repeat width height v
            , bbox = bbox
            , radius = radius
            }


{-| Apply a function to every element in the HexBin
-}
map : (a -> b) -> HexBin a -> HexBin b
map f (HexBin hexbin) =
    HexBin { hexbin | cells = Matrix.map f hexbin.cells }


{-| Apply a function to every cell  in the HexBin, using the `row` and `column` of the cell.
-}
indexedMap : (Int -> Int -> a -> b) -> HexBin a -> HexBin b
indexedMap f (HexBin hexbin) =
    HexBin { hexbin | cells = Matrix.indexedMap f hexbin.cells }


{-| Convenience function for updating many values at once
-}
updateMany : (a -> a) -> List ( Float, Float ) -> HexBin a -> HexBin a
updateMany mapper items hexbin =
    List.foldl (update mapper) hexbin items


{-| Update the HexBin with a new data point

The cell that the data point is placed in will be updated with the function that is the first argument.


    -- hexbin keeping track of the
    -- number of data points each cell
    hexbin : HexBin Int

    -- insert a data point with
    update (\current -> current + 1) (x, y) hexbin

If you want to overwrite a value, use

    insert : a -> ( Float, Float ) -> HexBin a -> HexBin a
    insert value ( x, y ) =
        update (\_ -> value) ( x, y )
-}
update : (a -> a) -> ( Float, Float ) -> HexBin a -> HexBin a
update mapper ( x, y ) (HexBin hexbin) =
    if BBox.contains (Vec2.fromTuple ( x, y )) hexbin.bbox then
        let
            ( row, column ) =
                toIndex hexbin.radius ( x, y )
        in
            HexBin { hexbin | cells = Matrix.update row column mapper hexbin.cells }
    else
        HexBin hexbin


{-| Get the value of a particular cell
-}
get : Int -> Int -> HexBin a -> Maybe a
get x y (HexBin { cells }) =
    Matrix.get x y cells


{-| Set the value of a particular cell. Returns an unchanged HexBin when the row or column is out of bounds.
-}
set : Int -> Int -> a -> HexBin a -> HexBin a
set x y v =
    modify x y (\_ -> v)


{-| Modify the value of a particular cell
-}
modify : Int -> Int -> (a -> a) -> HexBin a -> HexBin a
modify x y mapper (HexBin hexbin) =
    HexBin { hexbin | cells = Matrix.update x y mapper hexbin.cells }


{-| Convert a HexBin to a [`Matrix`](http://package.elm-lang.org/packages/eeue56/elm-flat-matrix/3.0.2/Matrix#Matrix)
-}
toMatrix : HexBin a -> Matrix a
toMatrix (HexBin { cells }) =
    cells


{-| Calculate the center of a hexagon. When converting to an image,
the hexagon has to be translated by this amount to be in the correct place
-}
hexagonTranslate : Int -> Int -> Float -> ( Float, Float )
hexagonTranslate row column radius =
    let
        height =
            radius * 2

        vert =
            height * 3 / 4

        width =
            sqrt (3) / 2 * height
    in
        if row % 2 == 1 then
            ( toFloat column * width, radius + toFloat row * vert )
        else
            ( toFloat column * width + width / 2, radius + toFloat row * vert )


{-| Convert a HexBin to a List
-}
toList : HexBin a -> List a
toList (HexBin { cells }) =
    cells |> Matrix.filter (\_ -> True) |> Array.toList


hexagonOrigin : Float -> List ( Float, Float )
hexagonOrigin radius =
    hexagon radius ( 0, 0 )


hexagon : Float -> ( Float, Float ) -> List ( Float, Float )
hexagon radius ( x, y ) =
    let
        thirdPi =
            pi / 3

        angles =
            [ 0, thirdPi, 2 * thirdPi, 3 * thirdPi, 4 * thirdPi, 5 * thirdPi ]

        helper angle =
            ( sin angle * radius + x
            , -(cos angle) * radius + y
            )
    in
        List.map helper angles


toIndex : Float -> ( Float, Float ) -> ( Int, Int )
toIndex radius ( x, y ) =
    let
        gridWidth =
            radius

        gridHeight =
            gridWidth

        c =
            gridHeight / 4

        halfWidth =
            gridWidth / 2

        row =
            round (y / gridHeight)

        oddRow =
            row % 2 == 1

        column =
            if oddRow then
                round ((x - halfWidth) / gridWidth)
            else
                round (x / gridWidth)

        relX =
            y - (toFloat row * gridHeight)

        relY =
            if oddRow then
                (x - (toFloat column * gridWidth)) - halfWidth
            else
                (x - (toFloat column * gridWidth))

        m =
            c / halfWidth
    in
        if relY < (-m * relX) + c then
            if oddRow then
                ( row - 1, column - 1 )
            else
                ( row - 1, column )
        else if relY < (m * relX) - c then
            if oddRow then
                ( row - 1, column + 1 )
            else
                ( row - 1, column )
        else
            ( row, column )



-- Render


{-| A config for rendering a HexBin

* `isEmpty`: predicate to determine whether a cell is empty
* `displayEmpty`: should empty cells be drawn?
* `borderColor`: Color of the border
* `borderWidth`: Width of the border
* `interpolateColor`: based on a cell's context, give a value between 0 and 1
* `fillColorMin`: Color of the cell when the color interpolation is 0
* `fillColorMax`: Color of the cell when the color interpolation is 1
* `modifyRadius`: Based on a cell's context, modify the radius it has when rendered
-}
type alias Config a =
    { isEmpty : a -> Bool
    , displayEmpty : Bool
    , borderColor : Color
    , borderWidth : Float
    , interpolateColor : { radius : Float, value : a, row : Int, column : Int } -> Float
    , fillColorMin : Color
    , fillColorMax : Color
    , modifyRadius : { radius : Float, value : a, row : Int, column : Int } -> Float
    }


{-| Turn the value of each cell into a color

* `largest`: value that is considered "full". The cell fill color will be `fillColorMax` when the value in the cell is this value.
-}
colorEncoding : Float -> Config Int
colorEncoding largest =
    { displayEmpty = False
    , borderColor = Color.black
    , borderWidth = 0.5
    , fillColorMin = Color.white
    , fillColorMax = Color.rgb 70 130 180
    , modifyRadius = \{ radius } -> radius
    , interpolateColor = \{ value } -> largest / 20
    , isEmpty = \cell -> cell == 0
    }


{-| Turn the value of each cell into an area

* `domain`: typically the minimal and maximal value a cell might have
* `range`: the minimal and maximal value the radius may have

The default uses a square root scale to convert the cell value (domain) into a radius (range).
-}
areaEncoding : ( Float, Float ) -> ( Float, Float ) -> Config Int
areaEncoding domain range =
    { displayEmpty = False
    , borderColor = Color.white
    , borderWidth = 0.5
    , fillColorMin = Color.rgb 70 130 180
    , fillColorMax = Color.rgb 70 130 180
    , modifyRadius = \{ value } -> convert 0.5 domain range value
    , interpolateColor = \_ -> 0
    , isEmpty = \cell -> cell == 0
    }


{-| Turn a HexBin into svg
-}
render : Config a -> HexBin a -> List (Svg msg)
render config ((HexBin { radius }) as hexbin) =
    hexbin
        |> indexedMap (renderHexagon config radius)
        |> toList


renderHexagon : Config a -> Float -> Int -> Int -> a -> Svg.Svg msg
renderHexagon config radius row column count =
    let
        t =
            hexagonTranslate row column radius

        toRadius =
            convert 0.5 ( 0, 50 ) ( 0, 20 )

        path =
            hexagonOrigin (config.modifyRadius { radius = radius, value = count, row = row, column = column })
                |> polyline

        fillColor =
            config.interpolateColor { radius = radius, value = count, row = row, column = column }
                |> interpolate LAB config.fillColorMin config.fillColorMax
                |> colorToHex
    in
        if config.displayEmpty || config.isEmpty count then
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



-- SQRT interpolation


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
