module Auth exposing (Model, Msg(..), init, update)

import Http
import Json.Decode
import Json.Encode
import Ports exposing (..)
import Time
import Utils exposing (..)


type Msg
    = NoOp
    | ChangePassword String
    | ChangeUserName String
    | FinishLogin (Result Http.Error String)
    | SendHeartbeat Time.Posix
    | StartLogin


type alias ServerUrl =
    String


type alias UserName =
    String


type alias Password =
    String


type alias Model =
    { lastError : String
    , password : String
    , sessionId : Maybe String
    , userName : String
    }


init : Maybe String -> Model
init sessionId =
    { lastError = ""
    , password = ""
    , sessionId = sessionId
    , userName = ""
    }


login : ServerUrl -> UserName -> Password -> Cmd Msg
login serverUrl userName password =
    let
        body =
            Http.jsonBody <|
                Json.Encode.object
                    [ ( "userName", Json.Encode.string userName )
                    , ( "password", Json.Encode.string password )
                    ]

        responseDecoder =
            Json.Decode.field "sessionId" Json.Decode.string
    in
    Http.post
        { url = serverUrl ++ "login"
        , body = body
        , expect = Http.expectJson FinishLogin responseDecoder
        }


sendHeartbeat : String -> Maybe String -> Cmd Msg
sendHeartbeat serverUrl sessionId =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "SessionId" <| Maybe.withDefault "" sessionId
            ]
        , url = serverUrl ++ "heartbeat"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectWhatever <| always NoOp
        }


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update serverUrl msg model =
    case msg of
        ChangePassword p ->
            ( { model | password = p }, Cmd.none )

        ChangeUserName name ->
            ( { model | userName = name }, Cmd.none )

        FinishLogin (Ok sessionId) ->
            ( { model
                | sessionId = Just sessionId
                , userName = ""
                , password = ""
              }
            , saveSessionId <| Just sessionId
            )

        FinishLogin (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        SendHeartbeat _ ->
            case model.sessionId of
                Just sessionId ->
                    ( model, sendHeartbeat serverUrl model.sessionId )

                Nothing ->
                    ( model, Cmd.none )

        StartLogin ->
            ( model, login serverUrl model.userName model.password )
