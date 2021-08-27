module Components exposing (TableFilter, defaultFilter, header, importPeeps, layouts, reviewTables)

import Csv.Decode as Decode
import Debug
import Dict exposing (Dict)
import Element exposing (Element, centerX, fill, height, maximum, paddingXY, px, rgb, scrollbarY, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Grid exposing (Params)
import Peeps exposing (Peep, Peeps)


section : String -> List (Element msg) -> Element msg
section title elems =
    let
        heading =
            Element.el [ Font.size 32 ] <| text title
    in
    Element.column [ paddingXY 0 16, spacing 8 ] <| heading :: elems


header =
    Element.el [ paddingXY 0 16, Font.size 48, Font.light ] <| text "Facepalm"


button attrs options =
    Input.button ([ paddingXY 16 16, spacing 8, Background.color <| rgb 0.9 0.9 0.9 ] ++ attrs) options


importPeeps : Maybe (Result Decode.Error Peeps) -> Maybe (Result Decode.Error Peeps) -> msg -> msg -> String -> (String -> msg) -> Element msg
importPeeps peeps updates selectList selectUpdates selectedClass selectClass =
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

        selectionInfo selection =
            case selection of
                Nothing ->
                    text "No selection"

                Just (Err err) ->
                    let
                        _ =
                            Debug.log "IMPORT ERROR" err
                    in
                    text <| "Error occurred while importing"

                Just (Ok { ok }) ->
                    text <| (String.fromInt <| List.length ok) ++ " selected"

        inputs =
            [ text "Select all files in the balfour folder"
            , button [ Input.focusedOnLoad ]
                { onPress = Just <| selectList
                , label = text "Import Balfour"
                }
            , selectionInfo peeps
            , button []
                { onPress = Just <| selectUpdates
                , label = text "Import Updates"
                }
            , selectionInfo updates
            ]

        elems =
            case peeps of
                Just (Ok _) ->
                    inputs ++ [ filterSelect ]

                _ ->
                    inputs
    in
    section "Import" elems


type alias TableFilter =
    { noPhoto : Bool
    , updatedPhoto : Bool
    , all : Bool
    }


defaultFilter =
    { noPhoto = True, updatedPhoto = True, all = False }


table : List Peep -> Dict String String -> Dict String String -> TableFilter -> (TableFilter -> msg) -> Element msg
table peeps photos updatedPhotos filter setFilter =
    let
        columns =
            [ { header = text "Pic"
              , width = Element.shrink
              , view =
                    \peep ->
                        Element.image [ height <| px 96 ]
                            { src = Maybe.withDefault "coconut.png" <| Dict.get peep.pic photos, description = "" }
              }
            , { header = text "Updated Pic"
              , width = Element.shrink
              , view =
                    \peep ->
                        case peep.preferredpic of
                            Nothing ->
                                Element.none

                            Just pic ->
                                Element.image [ height <| px 96 ]
                                    { src = Maybe.withDefault "coconut.png" <| Dict.get pic updatedPhotos, description = "" }
              }
            , { header = text "File", width = Element.fill, view = \peep -> text peep.pic }
            , { header = text "Last", width = Element.fill, view = \peep -> text peep.lastname }
            , { header = text "First", width = Element.fill, view = \peep -> text peep.firstname }
            , { header = text "Preferred", width = Element.fill, view = \peep -> text <| Maybe.withDefault "" peep.preferredname }
            , { header = text "Grade", width = Element.fill, view = \peep -> text peep.grade }
            ]
    in
    Element.table [] { data = peeps, columns = columns }


reviewTables : Peeps -> Dict String String -> Dict String String -> TableFilter -> (TableFilter -> msg) -> Element msg
reviewTables peeps photos updatedPhotos filter setFilter =
    let
        findPhoto : Peep -> Maybe String
        findPhoto peep =
            case peep.preferredpic of
                Just pic ->
                    Dict.get pic updatedPhotos

                Nothing ->
                    Dict.get peep.pic photos

        filterBoxes =
            [ ( "Missing Photo", .noPhoto, \show -> { filter | noPhoto = show } )
            , ( "Updated Photo", .updatedPhoto, \show -> { filter | updatedPhoto = show } )
            , ( "All", .all, \show -> { filter | all = show } )
            ]
                |> List.map
                    (\( label, getChecked, setChecked ) ->
                        Input.checkbox []
                            { onChange = setFilter << setChecked
                            , icon = Input.defaultCheckbox
                            , checked = getChecked filter
                            , label = Input.labelRight [] <| text label
                            }
                    )

        peepFilter peep =
            filter.all
                || (filter.noPhoto && findPhoto peep == Nothing)
                || (filter.updatedPhoto && peep.preferredpic /= Nothing)

        elems =
            filterBoxes
                ++ [ table (List.filter peepFilter peeps.ok) photos updatedPhotos filter setFilter
                   , table peeps.errors updatedPhotos Dict.empty filter setFilter
                   ]
    in
    section "Review Photos" elems


layouts : List Peep -> Dict String String -> Params -> msg -> Element msg
layouts peeps photos params export =
    let
        pages =
            Grid.toSvg params photos peeps
                |> List.map Element.html

        exportButton =
            [ text "Save this file inside the balfour folder"
            , button []
                { onPress = Just <| export
                , label = text "Export"
                }
            ]
    in
    section "Preview" (pages ++ exportButton)
