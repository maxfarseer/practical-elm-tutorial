port module Ports exposing (saveSessionId, dumpModel)

port saveSessionId : Maybe String -> Cmd msg

port dumpModel : (() -> msg) -> Sub msg