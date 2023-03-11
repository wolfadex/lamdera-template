module Pages exposing (..)

import AppUrl exposing (AppUrl)
import Dict
import Effect exposing (Effect)
import Html
import Page
import Page.Authenticate
import Page.Game
import Page.Home
import Route exposing (Route)
import SharedModel exposing (SharedModel)
import User exposing (User(..))
import View exposing (..)


type Msg
    = HomeMsg Page.Home.Msg
    | GameMsg Page.Game.Msg
    | AuthenticateMsg Page.Authenticate.Msg


type Page
    = Blank
    | Home Page.Home.Model
    | Game Page.Game.Model
    | Authenticate Page.Authenticate.Model


changeRouteTo : AppUrl -> Route -> User -> SharedModel -> Page -> ( Page, Effect Msg )
changeRouteTo url route user sharedModel page =
    case route of
        Route.Home ->
            Page.Home.init url.queryParameters sharedModel
                |> updateWith Home HomeMsg

        Route.Game ->
            Page.Game.init url.queryParameters sharedModel
                |> updateWith Game GameMsg

        Route.Authenticate ->
            Page.Authenticate.init url.queryParameters sharedModel
                |> updateWith Authenticate AuthenticateMsg



-- Effect.redirectWhenUnauthenticated session


update : SharedModel -> Msg -> Page -> ( Page, Effect Msg )
update sharedModel msg_ page =
    case ( msg_, page ) of
        ( HomeMsg msg, Home model ) ->
            Page.Home.update sharedModel msg model
                |> updateWith Home HomeMsg

        ( GameMsg msg, Game model ) ->
            Page.Game.update sharedModel msg model
                |> updateWith Game GameMsg

        ( AuthenticateMsg msg, Authenticate model ) ->
            Page.Authenticate.update sharedModel msg model
                |> updateWith Authenticate AuthenticateMsg

        _ ->
            ( page, Effect.none )


updateWith : (pageModel -> Page) -> (pageMsg -> Msg) -> Page.Update pageModel pageMsg -> ( Page, Effect Msg )
updateWith toPage toMsg pageUpdate =
    let
        data =
            Page.toData pageUpdate
    in
    ( toPage data.model
    , List.map (Effect.map toMsg) data.effects
        |> Effect.batch
    )


viewDocument : User -> Page -> { title : String, content : Html.Html msg } -> View msg
viewDocument session _ { title, content } =
    { title = "Sim Cards - " ++ title
    , content = content
    }


mapDocument : (msg1 -> msg2) -> View msg1 -> View msg2
mapDocument changeMsg { title, content } =
    { title = title, content = Html.map changeMsg content }


view : User -> SharedModel -> Page -> View Msg
view user sharedModel page =
    let
        viewPage : (pageMsg -> Msg) -> View pageMsg -> View Msg
        viewPage toPageMsg config =
            viewDocument user page config
                |> mapDocument toPageMsg
    in
    case page of
        Blank ->
            viewDocument user
                page
                { title = ""
                , content = Html.text ""
                }

        Home model ->
            viewPage HomeMsg (Page.Home.view model)

        Game model ->
            viewPage GameMsg (Page.Game.view model)

        Authenticate model ->
            viewPage AuthenticateMsg (Page.Authenticate.view model)


default : Page
default =
    Blank
