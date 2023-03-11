module Extra.Maybe exposing (applyMaybe)


applyMaybe : Maybe a -> (a -> b -> b) -> b -> b
applyMaybe maybe f b =
    case maybe of
        Just a ->
            f a b

        Nothing ->
            b
