port module Ports exposing (saveSessionId)

port saveSessionId : Maybe String -> Cmd msg
