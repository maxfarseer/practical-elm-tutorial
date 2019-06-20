module Types exposing (AppState, SessionId, asString)

import Auth


type SessionId
    = SessionId String


type alias AppState =
    { auth : Auth.Model
    , currPlanText : String
    , isMenuOpen : Bool
    , lastError : String
    , serverUrl : String
    }


asString : SessionId -> String
asString (SessionId id) =
    id
