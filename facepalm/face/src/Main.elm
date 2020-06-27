module Main exposing (main)

import Browser exposing (Document)
import Element exposing (Element, height, px, text)
import Element.Input as Input
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Grid exposing (Params)
import Html exposing (Html)
import Peeps exposing (Peep)
import Task


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { peeps : Maybe (Result (List Peeps.Error) (List Peep))
    , params : Grid.Params
    }


type Msg
    = Select
    | Selected File
    | Loaded String
    | Download String String String


init : () -> ( Model, Cmd Msg )
init =
    always
        ( { peeps = Nothing
          , params =
                { pagesize = { x = 54, y = 72 }
                , margins = { x = 4, y = 4 }
                , elemsize = { x = 5, y = 7 }
                , rows = 8
                , cols = 6
                , gutters = { x = 0.5, y = 1 }
                }
          }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select ->
            ( model, Select.file [ "text/txt" ] Selected )

        Selected file ->
            ( model, Task.perform Loaded (File.toString file) )

        Loaded contents ->
            ( { model | peeps = Just <| Peeps.fromBalfour contents }, Cmd.none )

        Download name mime string ->
            ( model, Download.string name mime string )


view : Model -> Document Msg
view model =
    let
        peepsView =
            case model.peeps of
                Nothing ->
                    text "Import file to get started"

                Just (Err errs) ->
                    text "errors importing"

                Just (Ok peeps) ->
                    viewPeeps model.params <|
                        List.filter (\peep -> peep.grade == "Junior") peeps

        body =
            [ text "Facepalm"
            , Input.button [] { onPress = Just Select, label = text "Import" }
            , peepsView
            ]
    in
    { title = "FacePalm"
    , body = [ Element.layout [] <| Element.column [] body ]
    }


viewPeeps : Params -> List Peep -> Element Msg
viewPeeps params peeps =
    let
        pics =
            { header = text "Pic"
            , width = Element.shrink
            , view =
                \peep ->
                    Element.image [ height <| px 64 ]
                        { src = peep.pic, description = "" }
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

        sorted =
            List.sortWith Peeps.cmp peeps
    in
    Element.column []
        [ Element.table [] { data = peeps, columns = [ pics, lastnames, firstnames, grades ] }
        , viewLayouts params sorted
        , Input.button []
            { onPress = Just <| Download "layout.json" "text/json" <| Grid.toJson params sorted
            , label = text "Export"
            }
        ]


viewLayouts : Params -> List Peep -> Element Msg
viewLayouts params peeps =
    let
        pages =
            Grid.toSvg params peeps
    in
    Element.column [] (List.map Element.html pages)