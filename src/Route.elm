module Route exposing
    ( Route(..)
    , parse
    , toString
    )

import AppUrl exposing (AppUrl)
import Url exposing (Url)


parse : Url -> Route
parse url =
    case (AppUrl.fromUrl url).path of
        [] ->
            Home

        [ "game" ] ->
            Game

        _ ->
            Home


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Game ->
            "/game"


type Route
    = Home
    | Game
