module Main exposing (main)

import Browser exposing (Document)
import Components as Component exposing (TableFilter)
import Debug exposing (log)
import Dict exposing (Dict)
import Element exposing (Element, centerX, fill, height, maximum, padding, px, scrollbarY, text, width)
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Grid exposing (Params)
import Peeps exposing (Peep)
import Task exposing (Task)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { photos : Dict String String
    , peeps : Maybe (Result (List Peeps.Error) (List Peep))
    , class : String
    , filter : TableFilter
    , params : Grid.Params
    }


initModel =
    { photos = Dict.empty
    , peeps = Nothing
    , params =
        { pagesize = { x = 54, y = 72 }
        , margins = { x = 4, y = 4 }
        , elemsize = { x = 5, y = 7 }
        , rows = 8
        , cols = 6
        , gutters = { x = 0.5, y = 1 }
        }
    , class = "Freshman"
    , filter = Component.defaultFilter
    }


type Msg
    = Dispatch (Cmd Msg)
    | Download String String String
    | SelectedFiles File (List File)
    | LoadedFiles (List FileResult)
    | SetClass String
    | SetFilter TableFilter


init : () -> ( Model, Cmd Msg )
init =
    always ( initModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


type FileResult
    = PeepsList String
    | Photo String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Dispatch cmd ->
            ( model, cmd )

        SelectedFiles first files ->
            let
                loadFile : File -> Task x FileResult
                loadFile =
                    \file ->
                        case File.mime file of
                            "text/plain" ->
                                Task.map PeepsList <| File.toString file

                            _ ->
                                Task.map (\contents -> Photo (File.name file) contents) <| File.toUrl file
            in
            ( model, Task.perform LoadedFiles <| Task.sequence <| List.map loadFile (first :: files) )

        LoadedFiles items ->
            let
                iterItems : FileResult -> ( Maybe String, List ( String, String ) ) -> ( Maybe String, List ( String, String ) )
                iterItems =
                    \item ( peeps, photos ) ->
                        case ( item, peeps ) of
                            ( PeepsList list, Nothing ) ->
                                ( Just list, photos )

                            ( Photo name contents, _ ) ->
                                ( peeps, ( name, contents ) :: photos )

                            _ ->
                                ( peeps, photos )
            in
            case List.foldr iterItems ( Nothing, [] ) items of
                ( Just peeps, photos ) ->
                    ( { model
                        | peeps = Just <| Peeps.fromBalfour peeps
                        , photos = Dict.fromList photos
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Download name mime string ->
            ( model, Download.string name mime string )

        SetClass by ->
            ( { model | class = by }, Cmd.none )

        SetFilter filter ->
            ( { model | filter = filter }, Cmd.none )


view : Model -> Document Msg
view model =
    let
        selection =
            Component.selection
                model.peeps
                (Dispatch <| Select.files [ "image/png", "image/jpg", "text/plain" ] SelectedFiles)
                model.class
                SetClass

        classPeeps =
            case model.peeps of
                Just (Ok peeps) ->
                    Just <| List.sortWith Peeps.cmp <| List.filter (\peep -> peep.grade == model.class) peeps

                _ ->
                    Nothing

        ( table, layouts ) =
            case classPeeps of
                Nothing ->
                    ( Element.none, Element.none )

                Just peeps ->
                    ( Component.table
                        peeps
                        model.photos
                        model.filter
                        SetFilter
                    , Component.layouts
                        peeps
                        model.photos
                        model.params
                        (Download "layout.json" "text/json" <| Grid.toJson model.params peeps)
                    )
    in
    { title = "FacePalm"
    , body =
        [ Element.layout [] <|
            Element.column
                [ centerX, padding 8, width (fill |> maximum 1200) ]
                [ Component.header
                , selection
                , table
                , layouts
                ]
        ]
    }
