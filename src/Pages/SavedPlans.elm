module Pages.SavedPlans exposing (Model, Msg(..), PageMsg(..), getSavedPlans, init, page, update)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Http
import PlanParsers.Json exposing (..)
import Utils exposing (httpErrorString)


type Msg
    = FinishSavedPlans (Result Http.Error (List SavedPlan))
    | ShowPlan String


type PageMsg
    = DisplayPlan String
    | DoNothing


type alias Model =
    { lastError : String
    , savedPlans : List SavedPlan
    }


init : String -> Maybe String -> ( Model, Cmd Msg )
init serverUrl sessionId =
    ( { lastError = "", savedPlans = [] }, getSavedPlans serverUrl sessionId )


getSavedPlans : String -> Maybe String -> Cmd Msg
getSavedPlans serverUrl sessionId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "SessionId" <| Maybe.withDefault "" sessionId ]
        , url = serverUrl ++ "plans"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson FinishSavedPlans decodeSavedPlans
        }


update : Msg -> Model -> ( Model, PageMsg )
update msg model =
    case msg of
        FinishSavedPlans (Ok savedPlans) ->
            ( { model | savedPlans = savedPlans }, DoNothing )

        FinishSavedPlans (Err error) ->
            ( { model | lastError = httpErrorString error }, DoNothing )

        ShowPlan planText ->
            ( model, DisplayPlan planText )


page : Model -> Element Msg
page model =
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
