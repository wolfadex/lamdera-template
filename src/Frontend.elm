module Frontend exposing (..)

import AppUrl
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Effect exposing (Effect)
import Game.Area
import Game.Card
import Game.Entity
import Html exposing (Html)
import Html.Attributes as Attr
import Lamdera
import Lamdera.Extra exposing (LamderaProgram)
import Page
import Page.Game
import Page.Home
import Pages
import Route exposing (Route)
import SharedModel exposing (SharedModel)
import Types exposing (..)
import Url exposing (Url)
import User exposing (User)
import View exposing (View)


app : LamderaProgram FrontendModel FrontendMsg ToFrontend
app =
    Effect.application
        { init = init
        , ignore = Ignored
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    Pages.view model.user model.sharedModel model.page
        |> Pages.mapDocument PageMsg
        |> toDocument


toDocument : View msg -> Browser.Document msg
toDocument v =
    { title = v.title
    , body = [ v.content ]
    }


viewMap : (msg1 -> msg2) -> Browser.Document msg1 -> Browser.Document msg2
viewMap toMsg v =
    { title = v.title
    , body = List.map (Html.map toMsg) v.body
    }


init : User -> Url -> Nav.Key -> ( FrontendModel, Effect FrontendMsg )
init user url key =
    let
        ( model, effect ) =
            changeRouteTo url
                (Route.parse url)
                { user = user
                , key = key
                , page = Pages.default
                , sharedModel = {}
                }
    in
    ( model
    , effect
    )


changeRouteTo : Url -> Route -> FrontendModel -> ( FrontendModel, Effect FrontendMsg )
changeRouteTo url route model =
    Pages.changeRouteTo url route model.user model.sharedModel model.page
        |> fromPage model


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Effect FrontendMsg )
update msg model =
    case msg of
        Ignored _ ->
            ( model, Effect.none )

        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Effect.pushUrl url )

                Browser.External href ->
                    ( model, Effect.loadUrl href )

        UrlChanged url ->
            changeRouteTo url (Route.parse url) model

        PageMsg pageMsg ->
            Pages.update model.sharedModel pageMsg model.page
                |> fromPage model


fromPage : FrontendModel -> ( Pages.Page, Effect Pages.Msg ) -> ( FrontendModel, Effect FrontendMsg )
fromPage model ( page, effect ) =
    ( { model | page = page }
    , Effect.map PageMsg effect
    )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.none


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Effect FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Effect.none )


whenAuthenticated : FrontendModel -> ( FrontendModel, Effect msg ) -> ( FrontendModel, Effect msg )
whenAuthenticated model whenAuthed =
    case model.user of
        User.Authenticated ->
            whenAuthed

        _ ->
            ( model, Effect.none )
