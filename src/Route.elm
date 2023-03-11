module Route exposing
    ( Route(..)
    , fromUrl
    , toString
    )

import AppUrl exposing (AppUrl)
import Dict
import Url exposing (Url)


fromUrl : AppUrl -> Route
fromUrl url =
    case url.path of
        [] ->
            Home

        [ "game" ] ->
            Game

        [ "authenticated" ] ->
            Authenticate

        _ ->
            Home


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Game ->
            "/game"

        Authenticate ->
            "/authenticated"


type Route
    = Home
    | Game
    | Authenticate
