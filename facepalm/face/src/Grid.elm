module Grid exposing (Params, toJson, toSvg)

import Debug exposing (log)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Encode as Encode
import Peeps exposing (Peep)
import Svg exposing (Svg, image, text, text_)
import Svg.Attributes exposing (dominantBaseline, fill, height, preserveAspectRatio, textAnchor, viewBox, width, x, xlinkHref, y)


type alias Params =
    { pagesize : Pair
    , margins : Pair
    , elemsize : Pair
    , rows : Int
    , cols : Int
    , gutters : Pair
    }


type alias Page =
    { side : Side
    , elems : List Elem
    }


type Elem
    = Pic Box String
    | Text Box Side String


type alias Box =
    { position : Pair
    , size : Pair
    }


type alias Pair =
    { x : Float, y : Float }


type Side
    = Left
    | Right


flip side =
    case side of
        Left ->
            Right

        Right ->
            Left


sideToString side =
    case side of
        Left ->
            "left"

        Right ->
            "right"


picsWidth p =
    toFloat p.cols * (p.elemsize.x + p.gutters.x)


placePic : Params -> Side -> Int -> Int -> Box
placePic p side row col =
    let
        x =
            toFloat col * (p.elemsize.x + p.gutters.x)

        x_base =
            case side of
                Left ->
                    -p.margins.x - picsWidth p + p.gutters.x

                Right ->
                    p.margins.x

        y =
            p.margins.y + toFloat row * (p.elemsize.y + p.gutters.y)
    in
    { position = { x = x_base + x, y = y }
    , size = p.elemsize
    }


placeLabel : Params -> Side -> Int -> Box
placeLabel p side row =
    let
        x =
            case side of
                Left ->
                    -p.pagesize.x

                Right ->
                    p.margins.x + picsWidth p

        y =
            p.margins.y + toFloat row * (p.elemsize.y + p.gutters.y)
    in
    { position = { x = x, y = y }
    , size = { x = p.pagesize.x - p.margins.x - picsWidth p, y = p.elemsize.y }
    }


layoutRow : Params -> Side -> Int -> List Peep -> ( List Elem, List Peep )
layoutRow p side row peeps_ =
    let
        peeps =
            List.take p.cols peeps_

        labelBox =
            placeLabel p side row

        labelText =
            peeps |> List.map Peeps.displayName |> String.join "\n"

        labelAlign =
            flip side

        pics =
            peeps
                |> List.indexedMap
                    (\col peep ->
                        Pic (placePic p side row col)
                            (Peeps.displayPic peep)
                    )
    in
    ( [ Text labelBox labelAlign labelText ] ++ pics
    , List.drop p.cols peeps_
    )


layoutPage : Params -> Side -> Int -> List Peep -> ( Page, List Peep )
layoutPage p side start peeps_ =
    let
        iter row peeps elems =
            if row == p.rows || peeps == [] then
                ( { side = side, elems = elems }, peeps )

            else
                let
                    ( rowElems, remaining ) =
                        layoutRow p side row peeps
                in
                iter (row + 1) remaining (elems ++ rowElems)
    in
    iter start peeps_ []


layout : Params -> List Peep -> List Page
layout p peeps_ =
    let
        iter side peeps pages =
            if peeps == [] then
                pages

            else
                let
                    ( pageElems, remaining ) =
                        layoutPage p side 0 peeps
                in
                iter (flip side) remaining (pages ++ [ pageElems ])

        ( titlePage, peeps__ ) =
            layoutPage p Left 2 peeps_
    in
    iter Right peeps__ [ titlePage ]


toSvg : Params -> List Peep -> List (Html msg)
toSvg p peeps =
    let
        textProps box align =
            case align of
                Left ->
                    [ x <| String.fromFloat <| 16 * box.position.x
                    , y <| String.fromFloat <| 16 * box.position.y
                    ]

                Right ->
                    [ x <| String.fromFloat <| 16 * (box.position.x + box.size.x)
                    , y <| String.fromFloat <| 16 * box.position.y
                    , textAnchor "end"
                    ]

        elemToSvg elem =
            case elem of
                Pic box pic ->
                    Svg.image
                        [ x <| String.fromFloat <| 16 * box.position.x
                        , y <| String.fromFloat <| 16 * box.position.y
                        , width <| String.fromFloat <| 16 * box.size.x
                        , height <| String.fromFloat <| 16 * box.size.y
                        , xlinkHref pic
                        , preserveAspectRatio "xMidYMid slice"
                        ]
                        []

                Text box align txt ->
                    Svg.text_
                        (textProps box align
                            ++ [ dominantBaseline "Hanging"
                               ]
                        )
                        [ text txt ]

        pageToSvg page =
            let
                viewLeft =
                    case page.side of
                        Left ->
                            -p.pagesize.x

                        Right ->
                            0
            in
            Svg.svg
                [ viewBox
                    ([ viewLeft, 0, p.pagesize.x, p.pagesize.y ]
                        |> List.map ((*) 16)
                        |> List.map String.fromFloat
                        |> String.join " "
                    )
                , width <| String.fromFloat <| 16 * p.pagesize.x
                , height <| String.fromFloat <| 16 * p.pagesize.y
                , style "border" "1px solid black"
                ]
                (List.map elemToSvg page.elems)

        pages =
            layout p peeps
    in
    List.map pageToSvg pages


toJson : Params -> List Peep -> String
toJson p peeps =
    let
        boxProps box =
            [ ( "pos", Encode.list Encode.float [ box.position.x, box.position.y ] )
            , ( "size", Encode.list Encode.float [ box.size.x, box.size.y ] )
            ]

        elemToJson elem =
            case elem of
                Pic box pic ->
                    Encode.object
                        ([ ( "type", Encode.string "pic" )
                         , ( "pic", Encode.string pic )
                         ]
                            ++ boxProps box
                        )

                Text box align txt ->
                    Encode.object
                        ([ ( "type", Encode.string "text" )
                         , ( "txt", Encode.string txt )
                         , ( "align", Encode.string <| sideToString align )
                         ]
                            ++ boxProps box
                        )

        pageToJson page =
            Encode.object
                [ ( "elems", Encode.list elemToJson page.elems ) ]

        pages =
            layout p peeps
    in
    Encode.encode 0 <|
        Encode.object
            [ ( "pages", Encode.list pageToJson pages ) ]
