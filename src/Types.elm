module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Pages
import SharedModel exposing (SharedModel)
import Url exposing (Url)
import User exposing (User)


type alias FrontendModel =
    { key : Key
    , user : User
    , sharedModel : SharedModel
    , page : Pages.Page
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Ignored String
    | PageMsg Pages.Msg


type ToBackend
    = BE_LoginOrSignup String


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
