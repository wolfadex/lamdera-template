module Effect exposing
    ( Effect
    , application
    , authenticate
    , batch
    , fromCmd
    , loadUrl
    , loginOrSignup
    , logout
    , map
    , none
    , pushRoute
    , pushUrl
    , redirectWhenUnauthenticated
    )

-- import Email exposing (Email)

import Api
import AppUrl
import Bridge exposing (ToBackend(..))
import Browser
import Browser.Navigation as Nav
import Dict
import Lamdera
import Lamdera.Extra exposing (LamderaProgram)
import Route exposing (Route)
import SharedModel exposing (SharedModel)
import Url exposing (Url)
import User exposing (User(..))


type Effect msg
    = None
    | FromCmd (Cmd msg)
    | Batch (List (Effect msg))
      -- Navigation
    | PushUrl Url
    | PushRoute Route
    | LoadUrl String
    | Logout
    | LoginOrSignup String
    | Authenticate String


application :
    { init : User -> Url -> Nav.Key -> ( { a | key : Nav.Key, sharedModel : SharedModel }, Effect msg )
    , view : { a | key : Nav.Key, sharedModel : SharedModel } -> Browser.Document msg
    , update : msg -> { a | key : Nav.Key, sharedModel : SharedModel } -> ( { a | key : Nav.Key, sharedModel : SharedModel }, Effect msg )
    , updateFromBackend : toFrontend -> { a | key : Nav.Key, sharedModel : SharedModel } -> ( { a | key : Nav.Key, sharedModel : SharedModel }, Effect msg )
    , ignore : String -> msg
    , subscriptions : { a | key : Nav.Key, sharedModel : SharedModel } -> Sub msg
    , onUrlChange : Url -> msg
    , onUrlRequest : Browser.UrlRequest -> msg
    }
    -> LamderaProgram { a | key : Nav.Key, sharedModel : SharedModel } msg toFrontend
application config =
    let
        init maybeSession =
            config.init (Maybe.withDefault Unauthenticated maybeSession)
    in
    Api.application
        { init = \maybeSession url key -> init maybeSession url key |> perform config.ignore
        , view = config.view
        , update = \msg model -> config.update msg model |> perform config.ignore
        , updateFromBackend = \msg model -> config.updateFromBackend msg model |> perform config.ignore
        , subscriptions = config.subscriptions
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        }


perform : (String -> msg) -> ( { a | key : Nav.Key, sharedModel : SharedModel }, Effect msg ) -> ( { a | key : Nav.Key, sharedModel : SharedModel }, Cmd msg )
perform ignore ( model, effect ) =
    case effect of
        None ->
            ( model, Cmd.none )

        FromCmd cmd ->
            ( model, cmd )

        Batch effects ->
            List.foldl (batchEffect ignore) ( model, [] ) effects
                |> Tuple.mapSecond Cmd.batch

        -- NAVIGATION
        PushUrl url ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        PushRoute route ->
            ( model, Nav.pushUrl model.key (Route.toString route) )

        LoadUrl href ->
            ( model, Nav.load href )

        Logout ->
            ( model, Lamdera.sendToBackend BE_Logout )

        LoginOrSignup email ->
            ( model, Lamdera.sendToBackend (BE_LoginOrSignup email) )

        Authenticate token ->
            ( model, Lamdera.sendToBackend (BE_Authenticate token) )


batchEffect : (String -> msg) -> Effect msg -> ( { a | key : Nav.Key, sharedModel : SharedModel }, List (Cmd msg) ) -> ( { a | key : Nav.Key, sharedModel : SharedModel }, List (Cmd msg) )
batchEffect ignore effect ( model, cmds ) =
    perform ignore ( model, effect )
        |> Tuple.mapSecond (\cmd -> cmd :: cmds)


{-| No effect.
-}
none : Effect msg
none =
    None


fromCmd : Cmd msg -> Effect msg
fromCmd =
    FromCmd


{-| Batch several effects together.
-}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


map : (a -> msg) -> Effect a -> Effect msg
map changeMsg effect =
    case effect of
        None ->
            None

        FromCmd cmd ->
            FromCmd (Cmd.map changeMsg cmd)

        Batch effects ->
            Batch (List.map (map changeMsg) effects)

        PushUrl url ->
            PushUrl url

        PushRoute route ->
            PushRoute route

        LoadUrl href ->
            LoadUrl href

        Logout ->
            Logout

        LoginOrSignup email ->
            LoginOrSignup email

        Authenticate token ->
            Authenticate token


pushUrl : Url -> Effect msg
pushUrl =
    PushUrl


pushRoute : Route -> Effect msg
pushRoute =
    PushRoute


{-| Leave the current page and load the given URL.
-}
loadUrl : String -> Effect msg
loadUrl href =
    LoadUrl href


loginOrSignup : String -> Effect msg
loginOrSignup =
    LoginOrSignup


authenticate : String -> Effect msg
authenticate =
    Authenticate


logout : Effect msg
logout =
    Logout


redirectWhenUnauthenticated : User -> Effect msg
redirectWhenUnauthenticated user =
    case user of
        User.Authenticated ->
            none

        User.Authenticating ->
            none

        User.Unauthenticated ->
            pushRoute Route.Home
