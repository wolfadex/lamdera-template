module Api exposing (..)

import Browser
import Browser.Navigation as Nav
import Lamdera
import Lamdera.Extra exposing (LamderaProgram)
import Url exposing (Url)
import User exposing (User)


application :
    { init : Maybe User -> Url -> Nav.Key -> ( model, Cmd msg )
    , onUrlChange : Url -> msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , updateFromBackend : toFrontend -> model -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    }
    -> LamderaProgram model msg toFrontend
application config =
    Lamdera.frontend
        { init = config.init Nothing
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , updateFromBackend = config.updateFromBackend
        , view = config.view
        }
