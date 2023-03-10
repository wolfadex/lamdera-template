module Page exposing (..)

import Effect exposing (Effect)
import Html exposing (Html)
import Lamdera


type Update model msg
    = Update { model : model, effects : List (Effect msg) }


update : model -> Update model msg
update model =
    Update { model = model, effects = [] }


type alias Page msg =
    { title : String
    , body : Html msg
    }


updateToModelEffect : Update model msg -> { model : model, effects : List (Effect msg) }
updateToModelEffect (Update data) =
    data


withEffect : Effect msg -> Update model msg -> Update model msg
withEffect effect (Update data) =
    Update { data | effects = effect :: data.effects }


toData : Update model msg -> { model : model, effects : List (Effect msg) }
toData (Update data) =
    data
