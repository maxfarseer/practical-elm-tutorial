module Types exposing (AppState, SessionId, asString, makeSessionId)

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


makeSessionId : String -> Maybe SessionId
makeSessionId s =
    if String.isEmpty s then
        Nothing

    else
        Just (SessionId s)


asString : SessionId -> String
asString (SessionId id) =
    id
