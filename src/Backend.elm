module Backend exposing (..)

import Dict
import Env
import Html
import Http
import Json.Decode
import Json.Encode
import Lamdera exposing (ClientId, SessionId)
import Route
import Set
import Task
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { sessions = Dict.empty }
    , Cmd.none
    )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect OnConnect
        , Lamdera.onDisconnect OnDisconnect
        ]


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        OnConnect sessionId clientId ->
            ( { model
                | sessions =
                    Dict.update sessionId
                        (\maybeSession ->
                            case maybeSession of
                                Nothing ->
                                    Just { clients = Set.singleton clientId, workOsProfile = Nothing }

                                Just session ->
                                    Just { session | clients = Set.insert clientId session.clients }
                        )
                        model.sessions
              }
            , case Dict.get sessionId model.sessions of
                Nothing ->
                    Cmd.none

                Just session ->
                    case session.workOsProfile of
                        Nothing ->
                            Cmd.none

                        Just _ ->
                            Lamdera.sendToFrontend clientId (RedirectTo Route.Game)
            )

        OnDisconnect sessionId clientId ->
            ( { model
                | sessions =
                    Dict.update sessionId
                        (\maybeSession ->
                            case maybeSession of
                                Nothing ->
                                    Nothing

                                Just session ->
                                    let
                                        remainingClients : Set.Set ClientId
                                        remainingClients =
                                            Set.remove clientId session.clients
                                    in
                                    if Set.isEmpty remainingClients then
                                        Nothing

                                    else
                                        Just { session | clients = remainingClients }
                        )
                        model.sessions
              }
            , Cmd.none
            )

        LoginOrSignupResponsed (Err err) ->
            ( model, Cmd.none )

        LoginOrSignupResponsed (Ok data) ->
            let
                _ =
                    Debug.log "data" data
            in
            ( model, Cmd.none )

        AuthenticateResponded _ (Err err) ->
            ( model, Cmd.none )

        AuthenticateResponded sessionId (Ok profile) ->
            case Dict.get sessionId model.sessions of
                Nothing ->
                    ( model, Cmd.none )

                Just session ->
                    ( { model
                        | sessions =
                            Dict.insert sessionId
                                { session | workOsProfile = Just profile }
                                model.sessions
                      }
                    , sendToSession (RedirectTo Route.Game) session
                    )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        BE_LoginOrSignup email ->
            ( model
            , createSession email
            )

        BE_Authenticate token ->
            ( model
            , authenticate sessionId token
            )


authenticate : SessionId -> String -> Cmd BackendMsg
authenticate sessionId code =
    Http.request
        { method = "POST"
        , url = workOsUrl ++ "/sso/token"
        , headers = [ workOsAuth ]
        , body =
            [ ( "client_id", Json.Encode.string Env.workosClientId )
            , ( "client_secret", Json.Encode.string Env.workosApiKey )
            , ( "grant_type", Json.Encode.string "authorization_code" )
            , ( "code", Json.Encode.string code )
            ]
                |> Json.Encode.object
                |> Http.jsonBody
        , expect = Http.expectJson (AuthenticateResponded sessionId) (Json.Decode.field "profile" decodeProfile)
        , timeout = Nothing
        , tracker = Nothing
        }


createSession : String -> Cmd BackendMsg
createSession email =
    Http.task
        { method = "POST"
        , url = workOsUrl ++ "/passwordless/sessions"
        , body =
            [ Just ( "email", Json.Encode.string email )
            , Just ( "type", Json.Encode.string "MagicLink" )
            , case Env.mode of
                Env.Development ->
                    Just ( "redirect_uri", Json.Encode.string "http://localhost:8000/authenticated" )

                Env.Production ->
                    Nothing
            ]
                |> List.filterMap identity
                |> Json.Encode.object
                |> Http.jsonBody
        , headers = [ workOsAuth ]
        , resolver = jsonResolver decodeWorkOsSession
        , timeout = Nothing
        }
        |> Task.andThen
            (\workOsSession ->
                Http.task
                    { method = "POST"
                    , headers = [ workOsAuth ]
                    , url = workOsUrl ++ "/passwordless/sessions/" ++ workOsSession.id ++ "/send"
                    , body =
                        [ ( "email", Json.Encode.string workOsSession.email )
                        , ( "type", Json.Encode.string "MagicLink" )
                        ]
                            |> Json.Encode.object
                            |> Http.jsonBody
                    , resolver = jsonResolver (Json.Decode.field "success" Json.Decode.bool)
                    , timeout = Nothing
                    }
            )
        |> Task.attempt LoginOrSignupResponsed


workOsUrl : String
workOsUrl =
    case Env.mode of
        Env.Development ->
            "http://localhost:8001/https://api.workos.com"

        Env.Production ->
            "https://api.workos.com"


workOsAuth : Http.Header
workOsAuth =
    Http.header "Authorization" ("Bearer " ++ Env.workosApiKey)


jsonResolver : Json.Decode.Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case Json.Decode.decodeString decoder body of
                        Err err ->
                            Err (Http.BadBody (Json.Decode.errorToString err))

                        Ok a ->
                            Ok a


sendToSession : ToFrontend -> Session -> Cmd BackendMsg
sendToSession toFrontendMsg session =
    session.clients
        |> Set.toList
        |> List.map (\clientId -> Lamdera.sendToFrontend clientId toFrontendMsg)
        |> Cmd.batch
