module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict
import Http
import Json.Decode
import Lamdera exposing (ClientId, SessionId)
import Pages
import Route
import Set
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
    { sessions : Dict.Dict SessionId Session
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Ignored String
    | PageMsg Pages.Msg


type ToBackend
    = BE_LoginOrSignup String
    | BE_Authenticate String


type BackendMsg
    = NoOpBackendMsg
    | OnConnect SessionId ClientId
    | OnDisconnect SessionId ClientId
    | LoginOrSignupResponsed (Result Http.Error Bool)
    | AuthenticateResponded SessionId (Result Http.Error WorkOsProfile)


type ToFrontend
    = NoOpToFrontend
    | RedirectTo Route.Route



--------------------------------------
--
--    TYPES
--
--------------------------------------


type alias Session =
    { clients : Set.Set ClientId
    , workOsProfile : Maybe WorkOsProfile
    }


type alias WorkOsSession =
    { id : String
    , email : String
    , expiresAt : String -- "2020-08-13T05:50:00.000Z"
    , link : String -- "https://auth.workos.com/passwordless/4TeRexuejWCKs9rrFOIuLRYEr/confirm"
    }


decodeWorkOsSession : Json.Decode.Decoder WorkOsSession
decodeWorkOsSession =
    Json.Decode.map4 WorkOsSession
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)
        (Json.Decode.field "expires_at" Json.Decode.string)
        (Json.Decode.field "link" Json.Decode.string)


type alias WorkOsProfile =
    { id : String -- "prof_01DMC79VCBZ0NY2099737PSVF1",
    , connectionId : String --"conn_01E4ZCR3C56J083X43JQXF3JK5",

    --  , connectionType : WorkOsConnectionType
    --  , organizationId : String -- "org_01EHWNCE74X7JSDV0X3SZ3KJNY",
    , email : String -- "todd@foo-corp.com",

    --  , firstName : String --"Todd",
    --  , lastName : String -- "Rundgren",
    , idpId : String -- "00u1a0ufowBJlzPlk357",
    }


decodeProfile : Json.Decode.Decoder WorkOsProfile
decodeProfile =
    Json.Decode.map4 WorkOsProfile
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "connection_id" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)
        (Json.Decode.field "idp_id" Json.Decode.string)


type WorkOsConnectionType
    = ADFSSAML
    | ADPOIDC
    | Auth0SAML
    | AzureSAML
    | CASSAML
    | ClassLinkSAML
    | CloudflareSAML
    | CyberArkSAML
    | DuoSAML
    | GenericOIDC
    | GenericSAML
    | GoogleOAuth
    | GoogleSAML
    | JumpCloudSAML
    | KeycloakSAML
    | LastPassSAML
    | LoginGovOIDC
    | MicrosoftOAuth
    | MiniOrangeSAML
    | NetIqSAML
    | OktaSAML
    | OneLoginSAML
    | OracleSAML
    | PingFederateSAML
    | PingOneSAML
    | SalesforceSAML
    | SimpleSamlPhpSAML
    | ShibbolethSAML
    | ShibbolethGenericSAML
    | VMwareSAML


decodeWorkOsConnectionType : Json.Decode.Decoder WorkOsConnectionType
decodeWorkOsConnectionType =
    Json.Decode.string
        |> Json.Decode.andThen
            (\type_ ->
                case type_ of
                    "ADFSSAML" ->
                        Json.Decode.succeed ADFSSAML

                    "ADPOIDC" ->
                        Json.Decode.succeed ADPOIDC

                    "Auth0SAML" ->
                        Json.Decode.succeed Auth0SAML

                    "AzureSAML" ->
                        Json.Decode.succeed AzureSAML

                    "CASSAML" ->
                        Json.Decode.succeed CASSAML

                    "ClassLinkSAML" ->
                        Json.Decode.succeed ClassLinkSAML

                    "CloudflareSAML" ->
                        Json.Decode.succeed CloudflareSAML

                    "CyberArkSAML" ->
                        Json.Decode.succeed CyberArkSAML

                    "DuoSAML" ->
                        Json.Decode.succeed DuoSAML

                    "GenericOIDC" ->
                        Json.Decode.succeed GenericOIDC

                    "GenericSAML" ->
                        Json.Decode.succeed GenericSAML

                    "GoogleOAuth" ->
                        Json.Decode.succeed GoogleOAuth

                    "GoogleSAML" ->
                        Json.Decode.succeed GoogleSAML

                    "JumpCloudSAML" ->
                        Json.Decode.succeed JumpCloudSAML

                    "KeycloakSAML" ->
                        Json.Decode.succeed KeycloakSAML

                    "LastPassSAML" ->
                        Json.Decode.succeed LastPassSAML

                    "LoginGovOIDC" ->
                        Json.Decode.succeed LoginGovOIDC

                    "MicrosoftOAuth" ->
                        Json.Decode.succeed MicrosoftOAuth

                    "MiniOrangeSAML" ->
                        Json.Decode.succeed MiniOrangeSAML

                    "NetIqSAML" ->
                        Json.Decode.succeed NetIqSAML

                    "OktaSAML" ->
                        Json.Decode.succeed OktaSAML

                    "OneLoginSAML" ->
                        Json.Decode.succeed OneLoginSAML

                    "OracleSAML" ->
                        Json.Decode.succeed OracleSAML

                    "PingFederateSAML" ->
                        Json.Decode.succeed PingFederateSAML

                    "PingOneSAML" ->
                        Json.Decode.succeed PingOneSAML

                    "SalesforceSAML" ->
                        Json.Decode.succeed SalesforceSAML

                    "SimpleSamlPhpSAML" ->
                        Json.Decode.succeed SimpleSamlPhpSAML

                    "ShibbolethSAML" ->
                        Json.Decode.succeed ShibbolethSAML

                    "ShibbolethGenericSAML" ->
                        Json.Decode.succeed ShibbolethGenericSAML

                    "VMwareSAML" ->
                        Json.Decode.succeed VMwareSAML

                    _ ->
                        Json.Decode.fail ("Unknown WorkOS connection type: " ++ type_)
            )
