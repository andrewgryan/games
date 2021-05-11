module Container exposing (style)

import Html exposing (Attribute)
import Html.Attributes exposing (class)


style : Attribute msg
style =
    class "max-w-sm pb-2 shadow-lg my-5 mx-2 rounded bg-gray-100"
