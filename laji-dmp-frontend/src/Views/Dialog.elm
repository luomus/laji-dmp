module Views.Dialog exposing (..)
import Html
import Html exposing (Html)
import Html.Attributes exposing (id)

-- https://www.lindsaykwardell.com/blog/native-dialog-in-elm
dialog : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
dialog elementId attr content =
    Html.node "dialog" ((id elementId) :: attr) content
