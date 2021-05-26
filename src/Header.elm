module Header exposing (view)

import Helper exposing (classes)
import Heroicons
import Html exposing (..)


view : Html msg
view =
    div
        [ classes
            [ "bg-teal-300"
            , "py-2"
            , "px-4"
            , "flex"
            , "flex-row"
            , "justify-between"
            , "items-center"
            , "text-gray-700"
            , "shadow-md"
            ]
        ]
        [ div
            [ classes
                [ "text-2xl"
                , "flex"
                , "flex-row"
                , "items-center"
                ]
            ]
            [ Heroicons.pencil
            , text "The Quiet Ryans"
            ]
        ]
