module Pages.Display exposing (Model, Msg(..), init, page, update)

import Attr
import Element exposing (..)
import Json.Decode
import PlanParsers.Json exposing (..)
import PlanTree


type Msg
    = MouseEnteredPlanNode Plan
    | MouseLeftPlanNode Plan


type alias Model =
    { selectedNode : Maybe Plan
    }


init : Model
init =
    { selectedNode = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseEnteredPlanNode plan ->
            { model | selectedNode = Just plan }

        MouseLeftPlanNode _ ->
            { model | selectedNode = Nothing }


page : { a | currPlanText : String } -> Model -> Element Msg
page appState model =
    let
        planTreeConfig =
            { onMouseEnteredNode = MouseEnteredPlanNode
            , onMouseLeftNode = MouseLeftPlanNode
            }
    in
    case Json.Decode.decodeString decodePlanJson appState.currPlanText of
        Ok planJson ->
            PlanTree.render planTreeConfig planJson model.selectedNode

        Err err ->
            el Attr.error <| text <| Json.Decode.errorToString err
