module Heroicons exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Svg exposing (path, svg)
import Svg.Attributes exposing (..)


sparkle : Html msg
sparkle =
    svg
        [ Svg.Attributes.class "h-6 w-6"
        , fill "none"
        , viewBox "0 0 24 24"
        , stroke "currentColor"
        ]
        [ Svg.path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth "2"
            , d "M5 3v4M3 5h4M6 17v4m-2-2h4m5-16l2.286 6.857L21 12l-5.714 2.143L13 21l-2.286-6.857L5 12l5.714-2.143L13 3z"
            ]
            []
        ]


pencil : Html msg
pencil =
    svg
        [ xmlSpace "http://www.w3.org/2000/svg"
        , fill "none"
        , viewBox "0 0 24 24"
        , stroke "currentColor"
        , Svg.Attributes.class "h-6"
        , Svg.Attributes.class "w-6"
        , Svg.Attributes.class "mr-4"
        ]
        [ Svg.path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth "2"
            , d "M15.232 5.232l3.536 3.536m-2.036-5.036a2.5 2.5 0 113.536 3.536L6.5 21.036H3v-3.572L16.732 3.732z"
            ]
            []
        ]


menu : Html msg
menu =
    svg
        [ xmlSpace "http://www.w3.org/2000/svg"
        , fill "none"
        , viewBox "0 0 24 24"
        , stroke "currentColor"
        ]
        [ Svg.path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth "2"
            , d "M4 6h16M4 12h16M4 18h16"
            ]
            []
        ]


chevronRight : Html msg
chevronRight =
    svg
        [ xmlSpace "http://www.w3.org/2000/svg"
        , Svg.Attributes.class "h-6 w-6"
        , fill "none"
        , viewBox "0 0 24 24"
        , stroke "currentColor"
        ]
        [ Svg.path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth "2"
            , d "M9 5l7 7-7 7"
            ]
            []
        ]


lockOpen : Html msg
lockOpen =
    svg
        [ Svg.Attributes.class "h-6 w-6"
        , fill "none"
        , viewBox "0 0 24 24"
        , stroke "currentColor"
        ]
        [ Svg.path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth "2"
            , d "M8 11V7a4 4 0 118 0m-4 8v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2z"
            ]
            []
        ]
