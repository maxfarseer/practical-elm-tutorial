module Main exposing (main)

import Attr
import Auth
import Browser
import Browser.Events exposing (onKeyPress)
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
import Pages.Display as Display
import Pages.Registration as Registration
import Pages.SavedPlans as SavedPlans
import PlanParsers.Json exposing (..)
import Ports
import Types exposing (AppState)


type alias SessionId =
    String


type Page
    = InputPage
    | DisplayPage Display.Model
    | LoginPage
    | RegistrationPage Registration.Model
    | SavedPlansPage SavedPlans.Model


type Msg
    = NoOp
    | ChangePlanText String
    | ToggleMenu
    | CreatePlan
    | Auth Auth.Msg
    | Display Display.Msg
    | RequestLogin
    | RequestSavedPlans SessionId
    | SavedPlans SavedPlans.Msg
    | FinishSavedPlans (Result Http.Error (List SavedPlan))
    | SubmitPlan
    | ShowPlan String
    | RequestLogout
    | DumpModel ()
    | RequestRegistration
    | Register Registration.Msg


type alias Model =
    { appState : AppState
    , currPage : Page
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
    { sessionId : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { appState =
            { auth = Auth.init flags.sessionId
            , currPlanText = ""
            , isMenuOpen = False
            , lastError = ""
            , serverUrl = "http://localhost:3000/"

            --          , selectedNode = Nothing
            --          , savedPlans = []
            }
      , currPage = InputPage
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.dumpModel DumpModel

        --        , Time.every (100 * 1000) SendHeartbeat
        , onKeyPress <| keyDecoder model
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ appState } as model) =
    case ( msg, model.currPage ) of
        ( Auth authMsg, _ ) ->
            let
                ( authModel, authCmd ) =
                    Auth.update serverUrl authMsg appState.auth

                currPage =
                    case authMsg of
                        Auth.FinishLogin (Ok _) ->
                            InputPage

                        _ ->
                            model.currPage
            in
            ( { model
                | appState = { appState | auth = authModel }
                , currPage = currPage
              }
            , Cmd.map Auth authCmd
            )

        ( ChangePlanText s, InputPage ) ->
            ( { model | appState = { appState | currPlanText = s } }, Cmd.none )

        ( CreatePlan, _ ) ->
            ( { model
                | appState = { appState | currPlanText = "" }
                , currPage = InputPage
              }
            , Cmd.none
            )

        ( RequestLogin, _ ) ->
            ( { model | currPage = LoginPage }, Cmd.none )

        ( RequestLogout, _ ) ->
            let
                auth =
                    appState.auth
            in
            ( { model
                | currPage = LoginPage
                , appState = { appState | auth = { auth | sessionId = Nothing } }
              }
            , Ports.saveSessionId <| Nothing
            )

        ( SubmitPlan, InputPage ) ->
            ( { model | currPage = DisplayPage Display.init }, Cmd.none )

        ( DumpModel (), _ ) ->
            ( Debug.log "model" model, Cmd.none )

        ( RequestSavedPlans sessionId, _ ) ->
            let
                ( pageModel, pageCmd ) =
                    SavedPlans.init appState.serverUrl sessionId
            in
            ( { model | currPage = SavedPlansPage pageModel }
            , Cmd.map SavedPlans pageCmd
            )

        ( SavedPlans pageMsg, SavedPlansPage pageModel ) ->
            let
                ( newPageModel, outMsg ) =
                    SavedPlans.update pageMsg pageModel

                newModel =
                    case outMsg of
                        SavedPlans.DisplayPlan planText ->
                            { model
                                | appState = { appState | currPlanText = planText }
                                , currPage = DisplayPage Display.init
                            }

                        _ ->
                            { model | currPage = SavedPlansPage newPageModel }
            in
            ( newModel, Cmd.none )

        ( RequestRegistration, _ ) ->
            ( { model | currPage = RegistrationPage Registration.init }, Cmd.none )

        ( Display pageMsg, DisplayPage pageModel ) ->
            let
                newPageModel =
                    Display.update pageMsg pageModel
            in
            ( { model | currPage = DisplayPage newPageModel }, Cmd.none )

        ( Register regMsg, RegistrationPage pageModel ) ->
            let
                ( regModel, regCmd, pageMsg ) =
                    Registration.update regMsg model.appState pageModel

                newModel =
                    case pageMsg of
                        Registration.FinishSuccessfully id ->
                            let
                                auth =
                                    appState.auth
                            in
                            { appState =
                                { appState | auth = { auth | sessionId = Just id } }
                            , currPage = InputPage
                            }

                        Registration.DoNothing ->
                            { model | currPage = RegistrationPage regModel }
            in
            ( newModel
            , Cmd.map Register regCmd
            )

        ( ToggleMenu, _ ) ->
            ( { model
                | appState = { appState | isMenuOpen = not appState.isMenuOpen }
              }
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )


serverUrl : String
serverUrl =
    "http://localhost:3000/"


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
            , text = model.appState.currPlanText
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


menuPanel : Model -> Element Msg
menuPanel model =
    let
        items =
            [ el [ pointer, onClick CreatePlan ] <| text "New plan" ]
                ++ (case model.appState.auth.sessionId of
                        Just sessionId ->
                            [ el [ pointer, onClick (RequestSavedPlans sessionId) ] <| text "Saved plans"
                            , el [ pointer, onClick RequestLogout ] <| text "Logout"
                            ]

                        Nothing ->
                            [ el [ pointer, onClick RequestLogin ] <| text "Login"
                            , el [ pointer, onClick RequestRegistration ] <| text "Registration"
                            ]
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
    if model.appState.isMenuOpen then
        row [ width fill, height fill ] [ overlay, panel ]

    else
        none


loginPage : Model -> Element Msg
loginPage model =
    column [ paddingXY 0 20, spacingXY 0 10, width (px 300), centerX ]
        [ Input.username Attr.input
            { onChange = Auth << Auth.ChangeUserName
            , text = model.appState.auth.userName
            , label = Input.labelAbove [] <| text "User name:"
            , placeholder = Nothing
            }
        , Input.currentPassword Attr.input
            { onChange = Auth << Auth.ChangePassword
            , text = model.appState.auth.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.button Attr.greenButton
            { onPress = Just <| Auth Auth.StartLogin
            , label = el [ centerX ] <| text "Login"
            }
        , el Attr.error <| text model.appState.lastError
        ]


keyDecoder : Model -> Decode.Decoder Msg
keyDecoder model =
    Decode.map2 Tuple.pair
        (Decode.field "altKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)
        |> Decode.andThen
            (\altAndShiftFlags ->
                case altAndShiftFlags of
                    ( True, True ) ->
                        Decode.field "code" Decode.string
                            |> Decode.map (keyToMsg model)

                    _ ->
                        Decode.succeed NoOp
            )


keyToMsg : Model -> String -> Msg
keyToMsg model key =
    case ( key, model.appState.auth.sessionId ) of
        ( "KeyS", Just id ) ->
            RequestSavedPlans id

        ( "KeyN", _ ) ->
            CreatePlan

        _ ->
            NoOp


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                DisplayPage pageModel ->
                    Display.page model.appState pageModel
                        |> Element.map Display

                InputPage ->
                    inputPage model

                LoginPage ->
                    loginPage model

                RegistrationPage pageModel ->
                    Registration.page pageModel
                        |> Element.map Register

                SavedPlansPage pageModel ->
                    SavedPlans.page pageModel
                        |> Element.map SavedPlans
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
