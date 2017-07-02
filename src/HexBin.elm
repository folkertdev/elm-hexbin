module HexBin exposing (..)

import Matrix exposing (Matrix)
import BoundingBox as BBox
import Vec2
import Array


type alias Model =
    Matrix Int


type HexBin a
    = HexBin
        { cells : Matrix a
        , bbox : BBox.BoundingBox
        , radius : Float
        }


radius : HexBin a -> Float
radius (HexBin hexbin) =
    hexbin.radius


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


map : (a -> b) -> HexBin a -> HexBin b
map f (HexBin hexbin) =
    HexBin { hexbin | cells = Matrix.map f hexbin.cells }


indexedMap : (Int -> Int -> a -> b) -> HexBin a -> HexBin b
indexedMap f (HexBin hexbin) =
    HexBin { hexbin | cells = Matrix.indexedMap f hexbin.cells }


insertManyWith : (a -> a) -> List ( Float, Float ) -> HexBin a -> HexBin a
insertManyWith mapper items hexbin =
    List.foldl (insertWith mapper) hexbin items


insertWith : (a -> a) -> ( Float, Float ) -> HexBin a -> HexBin a
insertWith mapper ( x, y ) (HexBin hexbin) =
    if BBox.contains (Vec2.fromTuple ( x, y )) hexbin.bbox then
        let
            ( row, column ) =
                toIndex hexbin.radius ( x, y )
        in
            HexBin { hexbin | cells = Matrix.update row column mapper hexbin.cells }
    else
        HexBin hexbin


insert : a -> ( Float, Float ) -> HexBin a -> HexBin a
insert value ( x, y ) =
    insertWith (\_ -> value) ( x, y )


get : Int -> Int -> HexBin a -> Maybe a
get x y (HexBin { cells }) =
    Matrix.get x y cells


set : Int -> Int -> a -> HexBin a -> HexBin a
set x y v =
    update x y (\_ -> v)


update : Int -> Int -> (a -> a) -> HexBin a -> HexBin a
update x y mapper (HexBin hexbin) =
    HexBin { hexbin | cells = Matrix.update x y mapper hexbin.cells }


toMatrix : HexBin a -> Matrix a
toMatrix (HexBin { cells }) =
    cells


fromMatrix : Float -> BBox.BoundingBox -> Matrix a -> HexBin a
fromMatrix radius bbox cells =
    HexBin { cells = cells, radius = radius, bbox = bbox }


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
