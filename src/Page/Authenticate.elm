module Page.Authenticate exposing (..)

import AppUrl exposing (QueryParameters)
import Dict
import Effect
import Extra.Maybe
import Game.Area
import Game.Card
import Html
import Html.Attributes
import Html.Events
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
    { code : Maybe String
    }


init : QueryParameters -> SharedModel -> Page.Update Model Msg
init queryParams _ =
    let
        code =
            queryParams
                |> Dict.get "code"
                |> Maybe.andThen List.head
    in
    { code = code }
        |> Page.update
        |> Extra.Maybe.applyMaybe code (Effect.authenticate >> Page.withEffect)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = LoginOrSignupSubmitted


update : SharedModel -> Msg -> Model -> Page.Update Model Msg
update sharedModel msg model =
    case msg of
        LoginOrSignupSubmitted ->
            model
                |> Page.update



-- |> Page.withEffect (Effect.loginOrSignup model.email)


view : Model -> View Msg
view model =
    { title = "Authenticate"
    , content =
        Html.div []
            [ Html.text (Maybe.withDefault "carl" model.code)
            ]
    }
