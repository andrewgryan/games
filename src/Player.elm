module Player exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Quiz exposing (Question)


type Player
    = Thinking
    | Done String


type Move
    = Forward
    | Wait


thinking : Player
thinking =
    Thinking


done : String -> Player
done statement =
    Done statement


chooseMove : Player -> List Player -> Move
chooseMove player players =
    case player of
        Thinking ->
            Wait

        Done statement ->
            if List.all (\p -> p == Done statement) players then
                Forward
                -- TODO add catch up logic

            else
                Wait



-- ENCODE


encode : Player -> Encode.Value
encode player =
    case player of
        Thinking ->
            Encode.object
                [ ( "type", Encode.string "THINKING" )
                ]

        Done statement ->
            Encode.object
                [ ( "type", Encode.string "DONE" )
                , ( "statement", Encode.string statement )
                ]



-- DECODE


decoder : Decoder Player
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "THINKING" ->
                        Decode.succeed Thinking

                    "DONE" ->
                        Decode.map Done (Decode.field "statement" Decode.string)

                    _ ->
                        Decode.fail "Not a valid Player encoding"
            )
