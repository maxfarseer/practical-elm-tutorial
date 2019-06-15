module Main exposing (main)

import Attr
import Browser
import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import PlanParsers.Json exposing (..)
import Ports


type Page
    = InputPage
    | DisplayPage
    | LoginPage
    | SavedPlansPage


type Msg
    = NoOp
    | ChangePlanText String
    | MouseEnteredPlanNode Plan
    | MouseLeftPlanNode Plan
    | ToggleMenu
    | CreatePlan
    | StartLogin
    | RequestLogin
    | FinishLogin (Result Http.Error String)
    | RequestSavedPlans
    | FinishSavedPlans (Result Http.Error (List SavedPlan))
    | SubmitPlan
    | ShowPlan String
    | ChangePassword String
    | ChangeUserName String
    | RequestLogout


type alias Model =
    { currPage : Page
    , currPlanText : String
    , selectedNode : Maybe Plan
    , isMenuOpen : Bool
    , password : String
    , userName : String
    , lastError : String
    , sessionId : Maybe String
    , savedPlans : List SavedPlan
    }


type alias PlanVersion =
    { version : Int
    , createdAt : String
    , planText : String
    }


type alias SavedPlan =
    { id : String
    , name : String
    , versions : List PlanVersion
    }


type alias Flags =
    { sessionId: Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currPage = InputPage
      , currPlanText = ""
      , selectedNode = Nothing
      , isMenuOpen = False
      , password = ""
      , userName = ""
      , lastError = ""
      , sessionId = flags.sessionId
      , savedPlans = []
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePassword s ->
            ( { model | password = s }, Cmd.none )

        ChangePlanText s ->
            ( { model | currPlanText = s }, Cmd.none )

        ChangeUserName s ->
            ( { model | userName = s }, Cmd.none )

        CreatePlan ->
            ( { model | currPage = InputPage, currPlanText = "" }, Cmd.none )

        MouseEnteredPlanNode plan ->
            ( { model | selectedNode = Just plan }, Cmd.none )

        MouseLeftPlanNode commonFields ->
            ( { model | selectedNode = Nothing }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        RequestLogin ->
            ( { model | currPage = LoginPage, password = "", userName = "" }, Cmd.none )

        StartLogin ->
            ( model, login model.userName model.password )

        FinishLogin (Ok sessionId) ->
            ( { model | sessionId = Just sessionId, currPage = InputPage }
            , Ports.saveSessionId <| Just sessionId)

        FinishLogin (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )

        SubmitPlan ->
            ( { model | currPage = DisplayPage }, Cmd.none )

        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        RequestSavedPlans ->
            ( { model | currPage = SavedPlansPage }, getSavedPlans model.sessionId )

        FinishSavedPlans (Ok savedPlans) ->
            ( { model | savedPlans = savedPlans }, Cmd.none )

        FinishSavedPlans (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )

        ShowPlan planText ->
            ( { model | currPlanText = planText, currPage = DisplayPage }, Cmd.none )

        RequestLogout ->
            ( { model | currPage = LoginPage, sessionId = Nothing}
            , Ports.saveSessionId <| Nothing )


navBar : Element Msg
navBar =
    row
        [ width fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color Color.blue
        ]
        [ el [ alignLeft ] <| text "VisExp"
        , Input.button (Attr.greyButton ++ [ padding 5, alignRight, width (px 80) ])
            { onPress = Just ToggleMenu
            , label = el [ centerX ] <| text "Menu"
            }
        ]


inputPage : Model -> Element Msg
inputPage model =
    column
        [ width (px 800)
        , spacingXY 0 10
        , centerX
        ]
        [ Input.multiline
            [ height (px 300)
            , Border.width 1
            , Border.rounded 3
            , Border.color Color.lightCharcoal
            , padding 3
            ]
            { onChange = ChangePlanText
            , text = model.currPlanText
            , placeholder = Nothing
            , label =
                Input.labelAbove [] <|
                    text "Paste the EXPLAIN output in JSON format:"
            , spellcheck = False
            }
        , Input.button
            (Attr.greenButton ++ [ width (px 200), height (px 40), alignRight ])
            { onPress = Just SubmitPlan
            , label = el [ centerX ] <| text "Go!"
            }
        ]


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network error"

        Http.Timeout ->
            "Request timeout"


decodePlanVersion : Decode.Decoder PlanVersion
decodePlanVersion =
    Decode.succeed PlanVersion
        |> required "version" Decode.int
        |> required "createdAt" Decode.string
        |> required "planText" Decode.string


decodeSavedPlans : Decode.Decoder (List SavedPlan)
decodeSavedPlans =
    Decode.list
        (Decode.succeed SavedPlan
            |> required "id" Decode.string
            |> required "name" Decode.string
            |> required "versions" (Decode.list decodePlanVersion)
        )


getSavedPlans : Maybe String -> Cmd Msg
getSavedPlans sessionId =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "SessionId" <| Maybe.withDefault "" sessionId ]
        , url = serverUrl ++ "plans"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson FinishSavedPlans decodeSavedPlans
        }


displayPage : Model -> Element Msg
displayPage model =
    let
        tree =
            case Decode.decodeString decodePlanJson model.currPlanText of
                Ok planJson ->
                    planNodeTree planJson.plan

                Err err ->
                    [ text <| Decode.errorToString err ]

        details =
            case model.selectedNode of
                Nothing ->
                    [ text "" ]

                Just plan ->
                    detailPanelContent plan
    in
    row [ width fill, paddingEach { top = 20, left = 0, right = 0, bottom = 0 } ]
        [ column [ width (fillPortion 7), height fill, alignTop ] tree
        , column
            [ width (fillPortion 3 |> maximum 500)
            , height fill
            , alignTop
            , padding 5
            , Border.widthEach { left = 1, right = 0, top = 0, bottom = 0 }
            , Border.color Color.grey
            ]
          <|
            details
        ]


detailPanelContent : Plan -> List (Element Msg)
detailPanelContent plan =
    let
        attr name value =
            wrappedRow [ width fill ]
                [ el
                    [ width (px 200)
                    , paddingEach { right = 10, left = 10, top = 3, bottom = 3 }
                    , alignTop
                    ]
                  <|
                    text name
                , paragraph [ width fill, Font.bold, scrollbarX ] [ text value ]
                ]

        header name =
            el [ paddingEach { top = 10, bottom = 5, left = 10, right = 0 } ] <|
                el
                    [ Font.bold
                    , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                    , Border.color lightGrey
                    ]
                <|
                    text name

        commonAttrs common =
            [ attr "Startup cost" <| String.fromFloat common.startupCost
            , attr "Total cost" <| String.fromFloat common.totalCost
            , attr "Schema" common.schema
            ]
    in
    case plan of
        PCte node ->
            commonAttrs node.common

        PGeneric node ->
            commonAttrs node

        PResult node ->
            commonAttrs node.common

        PSeqScan node ->
            commonAttrs node.common
                ++ [ header "Filter"
                   , attr "Filter" node.filter
                   , attr "Width" <| String.fromInt node.rowsRemovedByFilter
                   ]

        PSort node ->
            commonAttrs node.common
                ++ [ header "Sort"
                   , attr "Sort Key" <| String.join ", " node.sortKey
                   , attr "Sort Method" node.sortMethod
                   , attr "Sort Space Type" node.sortSpaceType
                   , attr "Sort Space Used" <| String.fromInt node.sortSpaceUsed
                   ]


childNodeTree : Plans -> Element Msg
childNodeTree (Plans plans) =
    column [ paddingEach { left = 20, right = 0, top = 0, bottom = 0 } ] <|
        List.concatMap planNodeTree plans


planNodeTree : Plan -> List (Element Msg)
planNodeTree plan =
    let
        nodeTypeEl nodeType =
            el [ Font.bold ] <| text nodeType

        treeNode node nodeDetails =
            [ el
                [ Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color lightBlue
                , mouseOver [ Background.color lightYellow ]
                , padding 4
                , onMouseEnter <| MouseEnteredPlanNode plan
                , onMouseLeave <| MouseLeftPlanNode plan
                ]
              <|
                paragraph [] (nodeTypeEl node.common.nodeType :: nodeDetails)
            , childNodeTree node.common.plans
            ]
    in
    case plan of
        PCte cteNode ->
            treeNode cteNode
                [ text " on "
                , el [ Font.italic ] <| text cteNode.cteName
                , text <| " (" ++ cteNode.alias_ ++ ")"
                ]

        PGeneric genericNode ->
            treeNode { common = genericNode }
                []

        PResult resultNode ->
            treeNode resultNode
                []

        PSeqScan seqScanNode ->
            treeNode seqScanNode
                [ text " on "
                , el [ Font.italic ] <| text seqScanNode.relationName
                , text <| " (" ++ seqScanNode.alias_ ++ ")"
                ]

        PSort sortNode ->
            treeNode sortNode
                [ text " on "
                , el [ Font.italic ] <| text <| String.join ", " sortNode.sortKey
                ]


menuPanel : Model -> Element Msg
menuPanel model =
    let
        items =
            [ el [ pointer, onClick CreatePlan ] <| text "New plan" ]
            ++ ( case model.sessionId of
                Just _ ->
                    [ el [ pointer, onClick RequestSavedPlans ] <| text "Saved plans"
                    , el [ pointer, onClick RequestLogout ] <| text "Logout" ]
                Nothing ->
                    [ el [ pointer, onClick RequestLogin ] <| text "Login" ]
            )

        panel =
            column
                [ Background.color Color.white
                , Border.widthEach { left = 1, right = 0, top = 0, bottom = 0 }
                , Border.color Color.grey
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 1
                    , blur = 10
                    , color = Color.lightCharcoal
                    }
                , Font.bold
                , Font.color Color.darkCharcoal
                , Font.family [ Font.sansSerif ]
                , width <| fillPortion 1
                , height fill
                , paddingXY 20 20
                , spacingXY 0 20
                ]
                items

        overlay =
            el [ width <| fillPortion 4, height fill, onClick ToggleMenu ] none
    in
    if model.isMenuOpen then
        row [ width fill, height fill ] [ overlay, panel ]

    else
        none


serverUrl : String
serverUrl =
    "http://localhost:3000/"


login : String -> String -> Cmd Msg
login userName password =
    let
        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "userName", Encode.string userName ), ( "password", Encode.string password ) ]

        responseDecoder =
            Decode.field "sessionId" Decode.string
    in
    Http.post
        { url = serverUrl ++ "login"
        , body = body
        , expect = Http.expectJson FinishLogin responseDecoder
        }


loginPage : Model -> Element Msg
loginPage model =
    column [ paddingXY 0 20, spacingXY 0 10, width (px 300), centerX ]
        [ Input.username Attr.input
            { onChange = ChangeUserName
            , text = model.userName
            , label = Input.labelAbove [] <| text "User name:"
            , placeholder = Nothing
            }
        , Input.currentPassword Attr.input
            { onChange = ChangePassword
            , text = model.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.button Attr.greenButton
            { onPress = Just StartLogin
            , label = el [ centerX ] <| text "Login"
            }
        , el Attr.error <| text model.lastError
        ]


savedPlansPage : Model -> Element Msg
savedPlansPage model =
    let
        annotateVersion name planVersion =
            { version = planVersion.version
            , planText = planVersion.planText
            , createdAt = planVersion.createdAt
            , name = name
            }

        annotateVersions savedPlan =
            List.map (annotateVersion savedPlan.name) savedPlan.versions

        tableAttrs =
            [ width (px 800)
            , paddingEach { top = 10, bottom = 50, left = 10, right = 10 }
            , spacingXY 10 10
            , centerX
            ]

        headerAttrs =
            [ Font.bold
            , Background.color Color.lightGrey
            , Border.color Color.darkCharcoal
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , centerX
            ]
    in
    table tableAttrs
        { data = List.concatMap annotateVersions model.savedPlans
        , columns =
            [ { header = el headerAttrs <| text "Plan name"
              , width = fill
              , view =
                    \plan ->
                        el
                            [ Font.underline
                            , mouseOver [ Font.color lightCharcoal ]
                            , onClick <| ShowPlan plan.planText
                            ]
                        <|
                            text plan.name
              }
            , { header = el headerAttrs <| text "Creation time"
              , width = fill
              , view = .createdAt >> text
              }
            , { header = el headerAttrs <| text "Version"
              , width = fill
              , view = .version >> String.fromInt >> text
              }
            ]
        }


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                DisplayPage ->
                    displayPage model

                InputPage ->
                    inputPage model

                LoginPage ->
                    loginPage model

                SavedPlansPage ->
                    savedPlansPage model
    in
    { title = "VisExp"
    , body =
        [ layout [ inFront <| menuPanel model ] <|
            column [ width fill, spacingXY 0 20 ]
                [ navBar
                , content
                ]
        ]
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
