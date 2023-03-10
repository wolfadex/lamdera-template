module Lamdera.Extra exposing (..)

import Browser
import Browser.Navigation
import Url exposing (Url)


type alias LamderaProgram model frontendMsg toFrontend =
    { init : Url -> Browser.Navigation.Key -> ( model, Cmd frontendMsg )
    , view : model -> Browser.Document frontendMsg
    , update : frontendMsg -> model -> ( model, Cmd frontendMsg )
    , updateFromBackend : toFrontend -> model -> ( model, Cmd frontendMsg )
    , subscriptions : model -> Sub frontendMsg
    , onUrlRequest : Browser.UrlRequest -> frontendMsg
    , onUrlChange : Url -> frontendMsg
    }
