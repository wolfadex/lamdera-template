module Bridge exposing (..)


type ToBackend
    = BE_LoginOrSignup String
    | BE_Logout
    | BE_Authenticate String
