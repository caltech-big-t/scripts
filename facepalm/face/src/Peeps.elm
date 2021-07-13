module Peeps exposing (Error, Peep, cmp, displayName, displayPic, fromBalfour)

import Csv
import Parser exposing (DeadEnd)
import Result exposing (Result(..))


type alias Peep =
    { lastname : String
    , firstname : String
    , grade : String
    , pic : String
    }


type Error
    = BadParse Int
    | Missing Int String


listIdx : Int -> List a -> Maybe a
listIdx i lst =
    List.head <| List.drop i lst


cmp : Peep -> Peep -> Order
cmp a b =
    let
        clean name =
            name |> String.toUpper |> String.filter Char.isUpper

        cmpName nameA nameB =
            compare (clean nameA) (clean nameB)
    in
    case compare a.grade b.grade of
        EQ ->
            case cmpName a.lastname b.lastname of
                EQ ->
                    cmpName a.firstname b.firstname

                order ->
                    order

        order ->
            order


displayName : Peep -> String
displayName peep =
    String.join ", " [ peep.lastname, peep.firstname ]


displayPic : Peep -> String
displayPic =
    .pic


fromRecord : List String -> Result String Peep
fromRecord record =
    let
        lastname =
            listIdx 4 record

        firstname =
            case listIdx 6 record of
                Just "" ->
                    listIdx 5 record

                name ->
                    name

        grade =
            listIdx 3 record

        pic =
            listIdx 2 record
    in
    case Maybe.map4 Peep lastname firstname grade pic of
        Just peep ->
            Ok peep

        Nothing ->
            Err "TODO"


fromBalfour : String -> Result (List Error) (List Peep)
fromBalfour csv =
    case Csv.parseWith '\t' csv of
        Err deadends ->
            Err <| List.map (\deadend -> BadParse deadend.row) deadends

        Ok { headers, records } ->
            let
                partition : Result String Peep -> ( List Peep, List Error, Int ) -> ( List Peep, List Error, Int )
                partition res ( succeeded_, failed_, i ) =
                    case res of
                        Ok peep ->
                            ( peep :: succeeded_, failed_, i + 1 )

                        Err msg ->
                            ( succeeded_, Missing i msg :: failed_, i + 1 )

                ( succeeded, failed, _ ) =
                    List.foldl partition ( [], [], 0 ) (List.map fromRecord records)
            in
            if List.length failed == 0 then
                Ok succeeded

            else
                Err failed
