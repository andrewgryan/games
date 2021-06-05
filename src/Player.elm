module Player exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Quiz exposing (Question)


type Player
    = Thinking Int
    | Done Int


type Move
    = Forward
    | Wait


thinking : Int -> Player
thinking questionIndex =
    Thinking questionIndex


done : Int -> Player
done questionIndex =
    Done questionIndex


chooseMove : Player -> List Player -> Move
chooseMove player players =
    case player of
        Thinking _ ->
            Wait

        Done questionIndex ->
            if List.all (\p -> p == Done questionIndex) players then
                Forward

            else if questionIndex < highestIndex players then
                Forward

            else
                Wait


highestIndex : List Player -> Int
highestIndex players =
    players
        |> List.map toIndex
        |> List.maximum
        |> Maybe.withDefault 0


toIndex : Player -> Int
toIndex player =
    case player of
        Thinking n ->
            n

        Done n ->
            n



-- ENCODE


encode : Player -> Encode.Value
encode player =
    case player of
        Thinking questionIndex ->
            Encode.object
                [ ( "type", Encode.string "THINKING" )
                , ( "questionIndex", Encode.int questionIndex )
                ]

        Done questionIndex ->
            Encode.object
                [ ( "type", Encode.string "DONE" )
                , ( "questionIndex", Encode.int questionIndex )
                ]



-- DECODE


decoder : Decoder Player
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "THINKING" ->
                        Decode.map Thinking (Decode.field "questionIndex" Decode.int)

                    "DONE" ->
                        Decode.map Done (Decode.field "questionIndex" Decode.int)

                    _ ->
                        Decode.fail "Not a valid Player encoding"
            )
