module PlanParsers.Json exposing (CommonFields, CteNode, Plan(..), PlanJson, Plans(..), ResultNode, SeqScanNode, SortNode, decodeCommonFields, decodeCteNode, decodeGenericNode, decodeNode, decodePlan, decodePlanJson, decodePlans, decodeResultNode, decodeSeqScanNode, decodeSortNode)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)


type Plan
    = PCte CteNode
    | PResult ResultNode
    | PSeqScan SeqScanNode
    | PSort SortNode
    | PGeneric CommonFields


type alias PlanJson =
    { executionTime : Float
    , plan : Plan
    , planningTime : Float
    , triggers : List String
    }


type Plans
    = Plans (List Plan)


type alias CommonFields =
    { nodeType : String
    , plans : Plans
    , relationName : String
    , schema : String
    , startupCost : Float
    , totalCost : Float
    }


type alias CteNode =
    { common : CommonFields
    , alias_ : String
    , cteName : String
    }


type alias ResultNode =
    { common : CommonFields
    , parentRelationship : String
    }


type alias SeqScanNode =
    { common : CommonFields
    , alias_ : String
    , filter : String
    , relationName : String
    , rowsRemovedByFilter : Int
    }


type alias SortNode =
    { common : CommonFields
    , sortKey : List String
    , sortMethod : String
    , sortSpaceUsed : Int
    , sortSpaceType : String
    }


decodePlanJson : Decode.Decoder PlanJson
decodePlanJson =
    Decode.succeed PlanJson
        |> optional "Execution Time" Decode.float 0
        |> required "Plan" decodePlan
        |> optional "Planning Time" Decode.float 0
        |> optional "Triggers" (Decode.list Decode.string) []


decodeResultNode : Decode.Decoder Plan
decodeResultNode =
    let
        innerDecoder =
            Decode.succeed ResultNode
                |> custom decodeCommonFields
                |> required "Parent Relationship" Decode.string
    in
    Decode.map PResult innerDecoder


decodeCommonFields : Decode.Decoder CommonFields
decodeCommonFields =
    Decode.succeed CommonFields
        |> required "Node Type" Decode.string
        |> optional "Plans" decodePlans (Plans [])
        |> optional "Relation Name" Decode.string ""
        |> optional "Schema" Decode.string ""
        |> required "Startup Cost" Decode.float
        |> required "Total Cost" Decode.float


decodePlans : Decode.Decoder Plans
decodePlans =
    Decode.map Plans <| Decode.list decodePlan


decodePlan : Decode.Decoder Plan
decodePlan =
    Decode.field "Node Type" Decode.string
        |> Decode.andThen decodeNode


decodeCteNode : Decode.Decoder Plan
decodeCteNode =
    let
        innerDecoder =
            Decode.succeed CteNode
                |> custom decodeCommonFields
                |> required "Alias" Decode.string
                |> required "CTE Name" Decode.string
    in
    Decode.map PCte innerDecoder


decodeSeqScanNode : Decode.Decoder Plan
decodeSeqScanNode =
    let
        innerDecoder =
            Decode.succeed SeqScanNode
                |> custom decodeCommonFields
                |> required "Alias" Decode.string
                |> optional "Filter" Decode.string ""
                |> required "Relation Name" Decode.string
                |> optional "Rows Removed by Filter" Decode.int 0
    in
    Decode.map PSeqScan innerDecoder


decodeSortNode : Decode.Decoder Plan
decodeSortNode =
    let
        innerDecoder =
            Decode.succeed SortNode
                |> custom decodeCommonFields
                |> required "Sort Key" (Decode.list Decode.string)
                |> required "Sort Method" Decode.string
                |> required "Sort Space Used" Decode.int
                |> required "Sort Space Type" Decode.string
    in
    Decode.map PSort innerDecoder


decodeGenericNode : Decode.Decoder Plan
decodeGenericNode =
    Decode.map PGeneric decodeCommonFields


decodeNode : String -> Decode.Decoder Plan
decodeNode nodeType =
    case nodeType of
        "CTE Scan" ->
            decodeCteNode

        "Result" ->
            decodeResultNode

        "Seq Scan" ->
            decodeSeqScanNode

        "Sort" ->
            decodeSortNode

        _ ->
            decodeGenericNode
