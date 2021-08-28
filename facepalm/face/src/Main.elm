module Main exposing (main)

import Browser exposing (Document)
import Components as Component exposing (TableFilter)
import Csv.Decode as Decode
import Debug exposing (log)
import Dict exposing (Dict)
import Element exposing (Element, centerX, fill, height, maximum, padding, px, scrollbarY, text, width)
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Grid exposing (Params)
import Peeps exposing (FileSet(..), Peep, Peeps)
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
    , updatedPhotos : Dict String String
    , peeps : Maybe (Result Decode.Error Peeps)
    , updatedPeeps : Maybe (Result Decode.Error Peeps)
    , class : String
    , filter : TableFilter
    , params : Grid.Params
    }


initModel =
    { photos = Dict.empty
    , updatedPhotos = Dict.empty
    , peeps = Nothing
    , updatedPeeps = Nothing
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
    | SelectedFiles FileSet File (List File)
    | LoadedFiles FileSet (List FileResult)
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
    | Invalid


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Dispatch cmd ->
            ( model, cmd )

        SelectedFiles which first files ->
            let
                loadFile : File -> Task x FileResult
                loadFile file =
                    case ( File.mime file, String.startsWith "image" <| File.mime file ) of
                        ( "text/plain", _ ) ->
                            Task.map PeepsList <| File.toString file

                        ( _, True ) ->
                            Task.map (\contents -> Photo (File.name file) contents) <| File.toUrl file

                        ( _, _ ) ->
                            Task.succeed Invalid
            in
            ( model, Task.perform (LoadedFiles which) <| Task.sequence <| List.map loadFile (first :: files) )

        LoadedFiles which items ->
            let
                iterItems : FileResult -> ( Maybe String, List ( String, String ) ) -> ( Maybe String, List ( String, String ) )
                iterItems item ( peeps, photos ) =
                    case ( item, peeps ) of
                        ( PeepsList list, Nothing ) ->
                            ( Just list, photos )

                        ( Photo name contents, _ ) ->
                            ( peeps, ( name, contents ) :: photos )

                        _ ->
                            ( peeps, photos )
            in
            case ( which, List.foldr iterItems ( Nothing, [] ) items ) of
                ( Originals, ( Just peeps, photos ) ) ->
                    ( { model
                        | peeps = Just <| Peeps.fromBalfour peeps
                        , photos = Dict.fromList photos
                      }
                    , Cmd.none
                    )

                ( Updates, ( Just peeps, photos ) ) ->
                    ( { model
                        | updatedPeeps = Just <| Peeps.fromUpdates peeps
                        , updatedPhotos = Dict.fromList photos
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
            Component.importPeeps
                model.peeps
                model.updatedPeeps
                (Dispatch <| Select.files [ "image/png", "image/jpg", "text/plain" ] (SelectedFiles Originals))
                (Dispatch <| Select.files [ "image/png", "image/jpg", "text/plain" ] (SelectedFiles Updates))
                model.class
                SetClass

        classPeeps =
            case ( model.peeps, model.updatedPeeps ) of
                ( Just (Ok peeps), Just (Ok updates) ) ->
                    Just <| Peeps.merge (Peeps.filterGrade model.class peeps) (Peeps.filterGrade model.class updates)

                ( Just (Ok peeps), _ ) ->
                    Just <| Peeps.filterGrade model.class peeps

                _ ->
                    Nothing

        ( table, layouts ) =
            case log "classpeeps" classPeeps of
                Nothing ->
                    ( Element.none, Element.none )

                Just peeps ->
                    ( Component.reviewTables
                        peeps
                        model.photos
                        model.updatedPhotos
                        model.filter
                        SetFilter
                    , Component.layouts
                        peeps.ok
                        model.photos
                        model.updatedPhotos
                        model.params
                        (Download "layout.json" "text/json" <| Grid.toJson model.params peeps.ok)
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
