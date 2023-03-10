module Page.Home exposing (..)

import AppUrl exposing (QueryParameters)
import Effect
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
    { email : String
    }


init : QueryParameters -> SharedModel -> Page.Update Model Msg
init _ _ =
    Page.update { email = "" }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = LoginOrSignupSubmitted
    | EmailChanged String


update : SharedModel -> Msg -> Model -> Page.Update Model Msg
update sharedModel msg model =
    case msg of
        LoginOrSignupSubmitted ->
            model
                |> Page.update
                |> Page.withEffect (Effect.loginOrSignup model.email)

        EmailChanged email ->
            Page.update { model | email = email }


view : Model -> View Msg
view model =
    { title = "Home"
    , content =
        Html.div []
            [ Html.form [ Html.Events.onSubmit LoginOrSignupSubmitted ]
                [ Html.input
                    [ Html.Attributes.type_ "email"
                    , Html.Attributes.value model.email
                    , Html.Events.onInput EmailChanged
                    ]
                    []
                , Html.button [] [ Html.text "Login or Signup" ]
                ]
            ]
    }
