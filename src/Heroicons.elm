module Heroicons exposing (chevronRight, menu, pencil)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Svg exposing (path, svg)
import Svg.Attributes exposing (..)


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
