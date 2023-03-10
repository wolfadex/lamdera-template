module Page.Game exposing (..)

import AppUrl exposing (QueryParameters)
import Game.Area
import Game.Card
import Html
import Html.Attributes
import Page exposing (Page)
import SharedModel exposing (SharedModel)
import View exposing (View)


layout =
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


type alias Model =
    {}


init : QueryParameters -> SharedModel -> Page.Update Model Msg
init _ _ =
    Page.update {}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NoOp


update : SharedModel -> Msg -> Model -> Page.Update Model Msg
update sharedModel msg model =
    case msg of
        NoOp ->
            Page.update {}


view : Model -> View Msg
view model =
    { title = "Game"
    , content =
        Html.div []
            [ Game.Area.pileAbove ( 10, 10 ) ( "draw deck", \attrs -> Game.Card.empty attrs "Draw Deck" ) []
                |> Game.Area.toHtml []
            ]
    }
