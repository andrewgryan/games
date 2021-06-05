module Player exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Quiz exposing (Question)


type Player
    = Thinking
    | Done String


thinking : Player
thinking =
    Thinking


done : String -> Player
done statement =
    Done statement


allDone : Player -> List Player -> Bool
allDone player players =
    case player of
        Thinking ->
            False

        Done statement ->
            List.all (\p -> p == Done statement) players



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
