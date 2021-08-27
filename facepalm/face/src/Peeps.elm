module Peeps exposing (Peep, Peeps, cmp, displayName, filterGrade, fromBalfour, fromUpdates, merge, preferredPic)

import Compare exposing (Comparator)
import Csv.Decode as Decode exposing (Decoder, FieldNames(..), field, map2, pipeline, string, succeed)
import Dict exposing (Dict)
import Parser exposing (DeadEnd)
import Result exposing (Result(..))


type alias Peep =
    { lastname : String
    , firstname : String
    , grade : String
    , preferredname : Maybe String
    , pic : String
    , preferredpic : Maybe String
    }


type alias PeepHash =
    ( String, String, String )


type alias Peeps =
    { ok : List Peep
    , errors : List Peep
    }


preferredName peep =
    Maybe.withDefault peep.firstname peep.preferredname


preferredPic peep =
    Maybe.withDefault peep.pic peep.preferredpic


normalizeName =
    -- normalizes for machine use/comparison.
    -- case insensitive, ignore symbols (but allow alphanumeric and whitespace)
    String.trim >> String.toUpper >> String.filter (\c -> Char.isUpper c || c == ' ')


hash peep =
    -- this is how we identify a person.
    -- we expect names to be unique within each grade
    ( peep.grade, normalizeName peep.lastname, normalizeName peep.firstname )


cmp : Comparator Peep
cmp =
    -- normalize, then compare by last, then preferred first name
    Compare.concat
        [ Compare.by .grade
        , Compare.compose .lastname <| Compare.by normalizeName
        , Compare.compose preferredName <| Compare.by normalizeName
        ]


displayName : Peep -> String
displayName peep =
    String.join ", " <| List.map String.trim [ peep.lastname, preferredName peep ]


filterGrade : String -> Peeps -> Peeps
filterGrade grade peeps =
    let
        peepMatch =
            .grade >> (==) grade
    in
    { ok = List.filter peepMatch peeps.ok, errors = List.filter peepMatch peeps.errors }



{-
   deduplicate : List Peep -> Peeps
   deduplicate =
       let
           iter : Peep -> Peeps -> Peeps
           iter peep deduped =
               if Dict.member (hash peep) deduped.ok then
                   { deduped | errors = Duplicated peep :: deduped.errors }

               else
                   { deduped | ok = Dict.insert (hash peep) peep deduped.ok }
       in
       List.foldl iter { ok = Dict.empty, errors = [] }

-}


merge : Peeps -> Peeps -> Peeps
merge originals updates =
    let
        iter : Peep -> Peeps -> Peeps
        iter update output =
            case List.partition (\peep -> hash peep == hash update) output.ok of
                ( [ match ], rest ) ->
                    let
                        merged =
                            { match | preferredpic = Just update.pic }
                    in
                    { output | ok = merged :: rest }

                _ ->
                    { output | errors = update :: output.errors }

        mergedPeeps =
            List.foldl iter originals updates.ok
    in
    { mergedPeeps | errors = mergedPeeps.errors ++ updates.errors }


decodeNonEmpty : Decode.Decoder String -> Decode.Decoder (Maybe String)
decodeNonEmpty decoder =
    let
        blankToNothing s =
            case s of
                "" ->
                    Nothing

                _ ->
                    Just s
    in
    Decode.map blankToNothing decoder


fromBalfour : String -> Result Decode.Error Peeps
fromBalfour csv =
    let
        decoder =
            Decode.into Peep
                |> pipeline (field "LastName" string)
                |> pipeline (field "First_Name" string)
                |> pipeline (field "Grade" string)
                |> pipeline (decodeNonEmpty <| field "NICK_NAME" string)
                |> pipeline (field "Image File Name" string)
                |> pipeline (succeed Nothing)

        results =
            Decode.decodeCustom
                { fieldSeparator = '\t'
                }
                FieldNamesFromFirstRow
                decoder
                csv
    in
    Result.map (\ok -> { ok = ok, errors = [] }) results


fromUpdates : String -> Result Decode.Error Peeps
fromUpdates csv =
    let
        decoder =
            Decode.into Peep
                |> pipeline (field "Last Name" string)
                |> pipeline (field "First Name" string)
                |> pipeline (field "Year" string)
                |> pipeline (decodeNonEmpty <| field "Preferred Name" string)
                |> pipeline (field "filename" string)
                |> pipeline (succeed Nothing)

        results : Result Decode.Error (List Peep)
        results =
            Decode.decodeCustom
                { fieldSeparator = ','
                }
                FieldNamesFromFirstRow
                decoder
                csv
    in
    Result.map (\ok -> { ok = ok, errors = [] }) results
