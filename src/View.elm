module View exposing (..)

import Html
import User exposing (User(..))


type alias View msg =
    { title : String
    , content : Html.Html msg
    }


procectedView : User -> View msg -> View msg
procectedView user view =
    case user of
        Unauthenticated ->
            { title = "Unavailable"
            , content = Html.text "This page is unavailbe without signing in"
            }

        Authenticating ->
            { title = "Authenticating"
            , content = Html.text "Authenticating..."
            }

        Authenticated ->
            view
