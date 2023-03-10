module Pages exposing (..)

import Dict
import Effect exposing (Effect)
import Html
import Page
import Page.Game
import Page.Home
import Route exposing (Route)
import SharedModel exposing (SharedModel)
import Url exposing (Url)
import User exposing (User(..))
import View exposing (..)


type Msg
    = HomeMsg Page.Home.Msg
    | GameMsg Page.Game.Msg


type Page
    = Blank
    | Home Page.Home.Model
    | Game Page.Game.Model


changeRouteTo : Url -> Route -> User -> SharedModel -> Page -> ( Page, Effect Msg )
changeRouteTo url route user sharedModel page =
    case route of
        Route.Home ->
            let
                carl : Page.Update Page.Home.Model Page.Home.Msg
                carl =
                    Page.Home.init Dict.empty sharedModel

                carl2 : Page.Update Page.Home.Model Page.Home.Msg -> ( Page, Effect Msg )
                carl2 =
                    updateWith Home HomeMsg
            in
            carl
                |> carl2

        Route.Game ->
            let
                carl : Page.Update Page.Game.Model Page.Game.Msg
                carl =
                    Page.Game.init Dict.empty sharedModel

                carl2 : Page.Update Page.Game.Model Page.Game.Msg -> ( Page, Effect Msg )
                carl2 =
                    updateWith Game GameMsg
            in
            carl
                |> carl2



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


default : Page
default =
    Blank
