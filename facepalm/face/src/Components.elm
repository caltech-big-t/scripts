module Components exposing (TableFilter, defaultFilter, header, layouts, selection, table)

import Dict exposing (Dict)
import Element exposing (Element, centerX, fill, height, maximum, paddingXY, px, scrollbarY, text, width)
import Element.Font as Font
import Element.Input as Input
import Grid exposing (Params)
import Peeps exposing (Peep)


section : String -> List (Element msg) -> Element msg
section title elems =
    let
        heading =
            Element.el [ Font.size 32 ] <| text title
    in
    Element.column [ paddingXY 0 16 ] <| heading :: elems


header =
    Element.el [ paddingXY 0 16, Font.size 48, Font.light ] <| text "Facepalm"


selection : Maybe (Result (List Peeps.Error) (List Peep)) -> msg -> String -> (String -> msg) -> Element msg
selection selectedList selectList selectedClass selectClass =
    let
        filterSelect =
            Input.radio []
                { onChange = selectClass
                , selected = Just selectedClass
                , label = Input.labelAbove [] (text "Filter")
                , options =
                    [ Input.option "Freshman" (text "Freshman")
                    , Input.option "Sophomore" (text "Sophomore")
                    , Input.option "Junior" (text "Junior")
                    , Input.option "Senior" (text "Senior")
                    ]
                }

        elems =
            case selectedList of
                Nothing ->
                    [ text "Import file to get started"
                    , Input.button [ Input.focusedOnLoad ]
                        { onPress = Just <| selectList
                        , label = text "Import Folder"
                        }
                    ]

                Just (Err errs) ->
                    [ text "errors importing" ]

                Just (Ok peeps) ->
                    [ filterSelect ]
    in
    section "Import" elems


type alias TableFilter =
    { noPhoto : Bool
    , modified : Bool
    }


defaultFilter =
    { noPhoto = True, modified = True }


table : List Peep -> Dict String String -> TableFilter -> (TableFilter -> msg) -> Element msg
table peeps photos filter setFilter =
    let
        hasPhoto peep =
            Dict.member peep.pic photos

        pics =
            { header = text "Pic"
            , width = Element.shrink
            , view =
                \peep ->
                    Element.image [ height <| px 64 ]
                        { src = Maybe.withDefault "coconut.png" <| Dict.get peep.pic photos, description = "" }
            }

        files =
            { header = text "File"
            , width = Element.fill
            , view = \peep -> text peep.pic
            }

        lastnames =
            { header = text "Last"
            , width = Element.fill
            , view = \peep -> text peep.lastname
            }

        firstnames =
            { header = text "First"
            , width = Element.fill
            , view = \peep -> text peep.firstname
            }

        grades =
            { header = text "Grade"
            , width = Element.fill
            , view = \peep -> text peep.grade
            }

        filtered =
            peeps
                |> List.filter (\peep -> not filter.noPhoto && hasPhoto peep)

        elems =
            [ Element.table [] { data = filtered, columns = [ pics, lastnames, firstnames, grades ] }
            ]
    in
    section "Match photos" elems


layouts : List Peep -> Dict String String -> Params -> msg -> Element msg
layouts peeps photos params export =
    let
        pages =
            Grid.toSvg params photos peeps
                |> List.map Element.html

        exportButton =
            Input.button []
                { onPress = Just <| export
                , label = text "Export"
                }
    in
    section "Preview" (pages ++ [ exportButton ])
