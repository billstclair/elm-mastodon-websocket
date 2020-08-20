----------------------------------------------------------------------
--
-- Main.elm
-- Example of using billstclair/elm-mastodon-websocket
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Dialog
import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , div
        , h2
        , hr
        , input
        , option
        , p
        , pre
        , select
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( autofocus
        , checked
        , cols
        , disabled
        , hidden
        , href
        , id
        , name
        , placeholder
        , readonly
        , rows
        , selected
        , size
        , style
        , target
        , type_
        , value
        )
import Html.Events exposing (onCheck, onClick, onInput, onMouseDown)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import JsonTree exposing (TaggedValue(..))
import List.Extra as LE
import Markdown
import Mastodon.EncodeDecode as ED
import Mastodon.Entity as Entity
    exposing
        ( Account
        , App
        , Authorization
        , Entity(..)
        , Instance
        )
import Mastodon.Login as Login exposing (FetchAccountOrRedirect(..))
import Mastodon.PortFunnels as PortFunnels exposing (FunnelDict, Handler(..), State)
import Mastodon.Request as Request
    exposing
        ( Error(..)
        , RawRequest
        , Request(..)
        , Response
        )
import Mastodon.WebSocket exposing (Event(..), StreamType(..))
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.WebSocket as WebSocket
import Regex
import String.Extra as SE
import Task
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as QP


type Started
    = NotStarted
    | StartedReadingModel
    | Started


type Dialog
    = NoDialog
    | ConfirmDialog String String Msg


type alias StreamResponse =
    { index : Int
    , response : String
    , event : Event
    , tree : Maybe JsonTree.Node
    , treeState : JsonTree.State
    }


type alias Stream =
    { index : Int
    , url : String
    , shown : Bool
    , responses : List StreamResponse
    , paused : Bool
    }


streamPredicate : Int -> Stream -> Bool
streamPredicate index stream =
    stream.index == index


streamResponsePredicate : Int -> StreamResponse -> Bool
streamResponsePredicate index response =
    response.index == index


type alias Model =
    { token : Maybe String
    , server : String
    , loginServer : Maybe String
    , showJsonTree : Bool
    , prettify : Bool
    , style : Style
    , showMetadata : Bool
    , showReceived : Bool
    , showEntity : Bool
    , username : String
    , accountId : String
    , maxStreamResponses : Int

    -- Non-persistent below here
    , dialog : Dialog
    , altKeyDown : Bool
    , request : Maybe RawRequest
    , response : Maybe Value
    , entity : Maybe Entity
    , responseTree : Result JD.Error JsonTree.Node
    , responseState : JsonTree.State
    , entityTree : Result JD.Error JsonTree.Node
    , entityState : JsonTree.State
    , selectedKeyPath : JsonTree.KeyPath
    , selectedKeyValue : String
    , metadata : Maybe Http.Metadata
    , savedModel : Maybe SavedModel
    , key : Key
    , url : Url
    , hideClientId : Bool
    , tokens : Dict String String
    , account : Maybe Account
    , instance : Maybe Instance
    , streams : List Stream
    , hashOrId : String

    -- Not input state
    , msg : Maybe String
    , started : Started
    , funnelState : State Msg
    }


type Msg
    = Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | SetResponseState JsonTree.State
    | SetEntityState JsonTree.State
    | SetStreamState Int Int JsonTree.State
    | ExpandAll WhichJson
    | CollapseAll WhichJson
    | SetDialog Dialog
    | OnKeyPress String
    | OnAltKey Bool
    | SetServer String
    | ClearSentReceived
    | TogglePrettify
    | ToggleShowJsonTree
    | ToggleShowMetadata
    | ToggleShowReceived
    | ToggleShowEntity
    | ToggleStyle
    | ToggleStreamShown Int
    | ToggleStreamPaused Int
    | RemoveStream Int
    | SetHashOrId String
    | AddStream StreamType
    | AddHashOrIdStream (String -> StreamType)
    | SetStreamResponseState Int Int JsonTree.State
    | ReceiveRedirect (Result ( String, Error ) ( String, App, Cmd Msg ))
    | ReceiveAuthorization (Result ( String, Error ) ( String, Authorization, Account ))
    | ReceiveInstance (Result Error Response)
    | ReceiveFetchAccount (Result ( String, Error ) ( String, String, Account ))
    | ReceiveGetVerifyCredentials (Result Error Response)
    | ReceiveAccountIdRelationships Bool (Result Error Response)
    | SetInstance (Result Error Response)
    | Process Value
    | SetLoginServer
    | Login
    | Logout
    | ClearAll
      -- All of the `SendXXX` messages receive their results via `ReceiveResponse`.
    | ReceiveResponse (Result Error Response)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


keyDecoder : Decoder Msg
keyDecoder =
    JD.field "key" JD.string
        |> JD.map OnKeyPress


altKeyDecoder : Bool -> Decoder Msg
altKeyDecoder down =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                if key == "Alt" then
                    JD.succeed <| OnAltKey down

                else
                    JD.fail "Not Alt key"
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PortFunnels.subscriptions model.funnelState Process model
        , if model.dialog /= NoDialog then
            Events.onKeyDown keyDecoder

          else
            Sub.batch
                [ Events.onKeyDown <| altKeyDecoder True
                , Events.onKeyUp <| altKeyDecoder False
                ]
        ]


emptyElement : String
emptyElement =
    "foo"


emptyUrl : Url
emptyUrl =
    { protocol = Url.Https
    , host = "example.com"
    , port_ = Nothing
    , path = "/" ++ emptyElement
    , query = Nothing
    , fragment = Nothing
    }


type alias CodeErrorState =
    { code : Maybe String
    , error : Maybe String
    , state : Maybe String
    }


parseQuery : String -> CodeErrorState
parseQuery queryString =
    let
        url =
            { emptyUrl | query = Just queryString }

        qp =
            QP.map3 CodeErrorState
                (QP.string "code")
                (QP.string "error")
                (QP.string "state")
    in
    Parser.parse (Parser.s emptyElement <?> qp) url
        |> Maybe.withDefault (CodeErrorState Nothing Nothing Nothing)


type CodeAndState
    = CodeAndState String (Maybe String)
    | CodeErrorAndState String (Maybe String)
    | NoCode


{-| This recognizes `?code=<code>&state=<state>` or `?error=<error>&state=<state>`

in the URL from the redirect from authentication.

-}
receiveCodeAndState : Url -> CodeAndState
receiveCodeAndState url =
    case url.query of
        Nothing ->
            NoCode

        Just q ->
            case parseQuery q of
                { code, error, state } ->
                    case code of
                        Just cod ->
                            case state of
                                Just st ->
                                    CodeAndState cod state

                                Nothing ->
                                    CodeErrorAndState "Missing state with code" code

                        Nothing ->
                            case error of
                                Just err ->
                                    CodeErrorAndState err state

                                Nothing ->
                                    NoCode


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init value url key =
    let
        hideClientId =
            case JD.decodeValue JD.bool value of
                Err _ ->
                    False

                Ok hide ->
                    hide

        ( code, state, msg ) =
            case receiveCodeAndState url of
                CodeAndState cod stat ->
                    ( Just cod, stat, Nothing )

                CodeErrorAndState m stat ->
                    ( Nothing, stat, Just m )

                NoCode ->
                    ( Nothing, Nothing, Nothing )
    in
    { token = Nothing
    , server = ""
    , loginServer = Nothing
    , prettify = True
    , style = LightStyle
    , username = ""
    , accountId = ""
    , maxStreamResponses = 5
    , showMetadata = False
    , showReceived = True
    , showEntity = False
    , showJsonTree = True

    -- Non-persistent below here
    , dialog = NoDialog
    , altKeyDown = False
    , request = Nothing
    , response = Nothing
    , entity = Nothing
    , responseTree = emptyJsonTree
    , responseState = JsonTree.defaultState
    , entityTree = emptyJsonTree
    , entityState = JsonTree.defaultState
    , selectedKeyPath = ""
    , selectedKeyValue = ""
    , metadata = Nothing
    , savedModel = Nothing
    , key = key
    , url = url
    , hideClientId = hideClientId
    , tokens = Dict.empty
    , account = Nothing
    , instance = Nothing
    , streams = []
    , hashOrId = ""

    -- Not input state
    , msg = msg
    , started = NotStarted
    , funnelState = initialFunnelState
    }
        -- As soon as the localStorage module reports in,
        -- we'll load the saved model,
        -- and then all the saved tokens.
        -- See `storageHandler` below, `get pk.model`.
        |> withCmds
            [ Navigation.replaceUrl key url.path
            , case ( code, state ) of
                ( Just cod, Just st ) ->
                    Login.getTokenTask { code = cod, state = st }
                        |> Task.attempt ReceiveAuthorization

                _ ->
                    Cmd.none
            ]


storageHandler : LocalStorage.Response -> State Msg -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if
                        LocalStorage.isLoaded state.storage
                            && (model.started == NotStarted)
                    then
                        StartedReadingModel

                    else
                        model.started
            }

        cmd =
            if
                (mdl.started == StartedReadingModel)
                    && (model.started == NotStarted)
            then
                Cmd.batch
                    [ get pk.model
                    , listKeysLabeled pk.token (pk.token ++ ".")
                    ]

            else
                Cmd.none
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            handleGetResponse label key value mdl

        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            handleListKeysResponse label prefix keys model

        _ ->
            mdl |> withCmd cmd


getInstance : Model -> Cmd Msg
getInstance model =
    let
        serverInfo =
            { server = model.server
            , token = Nothing
            }
    in
    Request.serverRequest (\id -> ReceiveInstance)
        []
        serverInfo
        ()
        (InstanceRequest Request.GetInstance)


getVerifyCredentials : Model -> Cmd Msg
getVerifyCredentials model =
    case model.loginServer of
        Nothing ->
            Cmd.none

        Just server ->
            case model.token of
                Nothing ->
                    sendRequest (InstanceRequest Request.GetInstance) model
                        |> Tuple.second

                Just token ->
                    Request.serverRequest (\_ -> ReceiveGetVerifyCredentials)
                        []
                        { server = server
                        , token = Just token
                        }
                        ()
                    <|
                        AccountsRequest Request.GetVerifyCredentials


handleListKeysResponse : Maybe String -> String -> List String -> Model -> ( Model, Cmd Msg )
handleListKeysResponse maybeLabel prefix keys model =
    case maybeLabel of
        Nothing ->
            model |> withNoCmd

        Just label ->
            -- label will be pk.token,
            -- but we won't care about that until the value comes in
            -- to handleGetResponse below.
            model |> withCmds (List.map (getLabeled label) keys)


handleGetModel : Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetModel maybeValue model =
    case maybeValue of
        Nothing ->
            { model
                | started = Started
                , msg = Nothing
            }
                |> withNoCmd

        Just value ->
            case JD.decodeValue savedModelDecoder value of
                Err err ->
                    { model
                        | started = Started
                        , msg =
                            Just <|
                                Debug.log "Error decoding SavedModel"
                                    (JD.errorToString err)
                    }
                        |> withNoCmd

                Ok savedModel ->
                    let
                        mdl =
                            Debug.log "savedModelToModel" <|
                                savedModelToModel savedModel model
                    in
                    { mdl
                        | started = Started
                        , msg = Nothing
                    }
                        |> withCmd
                            (if mdl.loginServer == Nothing then
                                Task.perform SetServer <| Task.succeed mdl.server

                             else
                                getVerifyCredentials mdl
                            )


handleGetToken : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetToken key value model =
    case JD.decodeValue JD.string value of
        Err err ->
            let
                ignore =
                    Debug.log ("Error decoding " ++ key) err
            in
            model |> withNoCmd

        Ok token ->
            let
                tokens =
                    model.tokens

                server =
                    Debug.log "Received token for server" <|
                        tokenStorageKeyServer key
            in
            { model | tokens = Dict.insert server token tokens }
                |> withNoCmd


handleGetResponse : Maybe String -> String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetResponse maybeLabel key maybeValue model =
    case maybeLabel of
        Nothing ->
            if key == pk.model then
                handleGetModel maybeValue model

            else
                model |> withNoCmd

        Just label ->
            case maybeValue of
                Nothing ->
                    model |> withNoCmd

                Just value ->
                    if label == pk.token then
                        handleGetToken key value model

                    else
                        model |> withNoCmd


socketHandler : WebSocket.Response -> State Msg -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }

        ( ( eventString, event ), k, msg ) =
            case response of
                WebSocket.ErrorResponse error ->
                    ( ( "", NoEvent ), "", Just <| WebSocket.errorToString error )

                WebSocket.ConnectedResponse { key, description } ->
                    ( ( "connected"
                      , ConnectedEvent description
                      )
                    , key
                    , Nothing
                    )

                WebSocket.ReconnectedResponse { key, description } ->
                    ( ( "connected"
                      , ReconnectedEvent description
                      )
                    , key
                    , Nothing
                    )

                WebSocket.MessageReceivedResponse { key, message } ->
                    ( ( message
                      , case
                            Mastodon.WebSocket.decodeEvent message
                        of
                            Err err ->
                                UnknownEvent message

                            Ok ev ->
                                ev
                      )
                    , key
                    , Nothing
                    )

                WebSocket.ClosedResponse { expected, reason, key } ->
                    let
                        string =
                            "Socket Closed, "
                                ++ (if expected then
                                        "expected"

                                    else
                                        "unexpected"
                                   )
                                ++ ", because: "
                                ++ reason
                    in
                    ( ( "closed"
                      , ClosedEvent string
                      )
                    , key
                    , Nothing
                    )

                _ ->
                    ( ( ""
                      , NoEvent
                      )
                    , ""
                    , Nothing
                    )
    in
    addResponse eventString event k model
        |> withNoCmd


maxResponses : Int
maxResponses =
    5


encodeEventValue : Int -> Event -> Value
encodeEventValue index event =
    let
        ( ev, value ) =
            case event of
                NoEvent ->
                    ( "nothing", JE.null )

                UpdateEvent status ->
                    ( "update", ED.encodeStatus status )

                NotificationEvent notification ->
                    ( "notification", ED.encodeNotification notification )

                DeleteEvent id ->
                    ( "delete", JE.string id )

                FiltersChangedEvent ->
                    ( "filters_changed", JE.null )

                ReconnectedEvent description ->
                    ( "reconnected", JE.string description )

                ConnectedEvent description ->
                    ( "connected", JE.string description )

                UnknownEvent string ->
                    ( "unknown", JE.string string )

                ClosedEvent string ->
                    ( "closed", JE.string string )
    in
    JE.object
        [ ( "_index", JE.int index )
        , ( "event", JE.string ev )
        , ( "payload", value )
        ]


addResponse : String -> Event -> String -> Model -> Model
addResponse message event key model =
    case String.toInt key of
        Nothing ->
            model

        Just index ->
            let
                streams =
                    model.streams
            in
            case LE.find (streamPredicate index) streams of
                Nothing ->
                    model

                Just stream ->
                    if stream.paused then
                        model

                    else
                        let
                            responseIndex =
                                case List.head stream.responses of
                                    Nothing ->
                                        0

                                    Just head ->
                                        head.index + 1

                            ( tree, state ) =
                                let
                                    value =
                                        encodeEventValue responseIndex event

                                    result =
                                        JsonTree.parseValue value
                                in
                                case result of
                                    Err _ ->
                                        ( Nothing, JsonTree.defaultState )

                                    Ok root ->
                                        ( Just root
                                        , JsonTree.collapseToDepth 1
                                            root
                                            JsonTree.defaultState
                                        )

                            response =
                                { index = responseIndex
                                , response = message
                                , event = event
                                , tree = tree
                                , treeState = state
                                }
                        in
                        { model
                            | streams =
                                LE.setIf (streamPredicate index)
                                    { stream
                                        | responses =
                                            (response :: stream.responses)
                                                |> List.take maxResponses
                                    }
                                    streams
                        }


emptyJsonTree : Result JD.Error JsonTree.Node
emptyJsonTree =
    JsonTree.parseString "[]"


updateJsonTrees : Model -> Model
updateJsonTrees model =
    let
        parse : Maybe Value -> ( Result JD.Error JsonTree.Node, JsonTree.State )
        parse value =
            case value of
                Nothing ->
                    ( emptyJsonTree, JsonTree.defaultState )

                Just v ->
                    let
                        result =
                            JsonTree.parseValue v
                    in
                    ( result
                    , case result of
                        Err _ ->
                            JsonTree.defaultState

                        Ok root ->
                            JsonTree.collapseToDepth 1 root JsonTree.defaultState
                    )

        ( responseTree, responseState ) =
            parse model.response

        ( entityTree, entityState ) =
            case model.entity of
                Nothing ->
                    parse Nothing

                Just entity ->
                    parse (Just <| ED.encodeEntity entity)
    in
    { model
        | responseTree = responseTree
        , responseState = responseState
        , entityTree = entityTree
        , entityState = entityState
        , selectedKeyPath = ""
        , selectedKeyValue = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( model2, cmd ) =
            updateInternal msg model

        savedModel =
            modelToSavedModel model2

        needsSaving =
            if model2.started /= Started then
                False

            else
                case model2.savedModel of
                    Nothing ->
                        True

                    Just sm ->
                        savedModel /= sm
    in
    { model2
        | savedModel =
            if needsSaving then
                Just savedModel

            else
                model2.savedModel
    }
        |> withCmds
            [ cmd
            , if needsSaving then
                put pk.model (Just <| encodeSavedModel savedModel)

              else
                Cmd.none
            ]


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        OnUrlRequest _ ->
            model |> withNoCmd

        OnUrlChange _ ->
            model |> withNoCmd

        SetResponseState state ->
            { model | responseState = state }
                |> withNoCmd

        SetEntityState state ->
            { model | entityState = state }
                |> withNoCmd

        SetStreamState streamIndex responseIndex state ->
            -- TODO
            model |> withNoCmd

        ExpandAll whichJson ->
            (case whichJson of
                ResponseJson ->
                    { model
                        | responseState =
                            JsonTree.expandAll model.responseState
                    }

                DecodedJson ->
                    { model
                        | entityState =
                            JsonTree.expandAll model.entityState
                    }

                StreamJson { streamIndex, response } ->
                    updateStreams model
                        streamIndex
                        response.index
                        (\resp ->
                            { response
                                | treeState =
                                    JsonTree.expandAll resp.treeState
                            }
                        )
            )
                |> withNoCmd

        CollapseAll whichJson ->
            (case whichJson of
                ResponseJson ->
                    case model.responseTree of
                        Ok root ->
                            { model
                                | responseState =
                                    JsonTree.collapseToDepth 1 root model.responseState
                            }

                        Err _ ->
                            model

                DecodedJson ->
                    case model.entityTree of
                        Ok root ->
                            { model
                                | entityState =
                                    JsonTree.collapseToDepth 1 root model.entityState
                            }

                        Err _ ->
                            model

                StreamJson { streamIndex, response } ->
                    case response.tree of
                        Nothing ->
                            model

                        Just root ->
                            updateStreams model
                                streamIndex
                                response.index
                                (\resp ->
                                    { resp
                                        | treeState =
                                            JsonTree.collapseToDepth 1
                                                root
                                                response.treeState
                                    }
                                )
            )
                |> withNoCmd

        SetDialog dialog ->
            { model | dialog = dialog }
                |> withCmd
                    (if model.dialog == NoDialog then
                        Cmd.none

                     else
                        Task.attempt (\_ -> Noop) <|
                            Dom.focus cancelButtonId
                    )

        OnKeyPress key ->
            { model
                | dialog =
                    if key == "Escape" then
                        NoDialog

                    else
                        model.dialog
            }
                |> withNoCmd

        OnAltKey isDown ->
            { model
                | altKeyDown = isDown
            }
                |> withNoCmd

        SetServer server ->
            let
                mdl =
                    { model | server = server }
            in
            mdl
                |> withCmd
                    (if String.contains "." server then
                        getInstance mdl

                     else
                        Cmd.none
                    )

        ClearSentReceived ->
            { model
                | request = Nothing
                , response = Nothing
                , entity = Nothing
                , metadata = Nothing
                , selectedKeyPath = ""
                , selectedKeyValue = ""
                , streams = []
            }
                |> withNoCmd

        TogglePrettify ->
            { model | prettify = not model.prettify }
                |> withNoCmd

        ToggleShowJsonTree ->
            let
                mdl =
                    if model.showJsonTree then
                        { model
                            | selectedKeyPath = ""
                            , selectedKeyValue = ""
                        }

                    else
                        model
            in
            { mdl | showJsonTree = not model.showJsonTree }
                |> withNoCmd

        ToggleShowMetadata ->
            { model | showMetadata = not model.showMetadata }
                |> withNoCmd

        ToggleShowReceived ->
            { model | showReceived = not model.showReceived }
                |> withNoCmd

        ToggleShowEntity ->
            { model | showEntity = not model.showEntity }
                |> withNoCmd

        ToggleStyle ->
            { model
                | style =
                    if model.style == LightStyle then
                        DarkStyle

                    else
                        LightStyle
            }
                |> withNoCmd

        ToggleStreamShown index ->
            case LE.find (streamPredicate index) model.streams of
                Nothing ->
                    model |> withNoCmd

                Just stream ->
                    { model
                        | streams =
                            LE.setIf (streamPredicate index)
                                { stream | shown = not stream.shown }
                                model.streams
                    }
                        |> withNoCmd

        ToggleStreamPaused index ->
            case LE.find (streamPredicate index) model.streams of
                Nothing ->
                    model |> withNoCmd

                Just stream ->
                    { model
                        | streams =
                            LE.setIf (streamPredicate index)
                                { stream | paused = not stream.paused }
                                model.streams
                    }
                        |> withNoCmd

        RemoveStream index ->
            let
                message =
                    WebSocket.makeClose <| String.fromInt index
            in
            { model
                | streams =
                    LE.filterNot (streamPredicate index) model.streams
            }
                |> withCmd (webSocketSend message)

        SetHashOrId hashOrId ->
            { model | hashOrId = hashOrId }
                |> withNoCmd

        AddStream streamType ->
            addStream streamType model

        AddHashOrIdStream streamTyper ->
            addStream (streamTyper model.hashOrId) model

        SetStreamResponseState streamIndex index state ->
            case LE.find (streamPredicate streamIndex) model.streams of
                Nothing ->
                    model |> withNoCmd

                Just stream ->
                    case LE.find (streamResponsePredicate index) stream.responses of
                        Nothing ->
                            model |> withNoCmd

                        Just response ->
                            { model
                                | streams =
                                    LE.setIf (streamPredicate streamIndex)
                                        { stream
                                            | responses =
                                                LE.setIf (streamResponsePredicate index)
                                                    { response | treeState = state }
                                                    stream.responses
                                        }
                                        model.streams
                            }
                                |> withNoCmd

        ReceiveRedirect result ->
            case result of
                Err ( server, err ) ->
                    ( { model | msg = Just <| Debug.toString err }
                    , Cmd.none
                    )

                Ok ( server, app, cmd ) ->
                    { model | msg = Nothing }
                        |> withCmd cmd

        ReceiveAuthorization result ->
            case result of
                Err ( server, err ) ->
                    ( { model | msg = Just <| Debug.toString err }
                    , Cmd.none
                    )

                Ok ( server, authorization, account ) ->
                    let
                        ( mdl, cmd ) =
                            saveAuthorization server authorization model

                        serverInfo =
                            { server = server
                            , token = Just authorization.token
                            }

                        mdl2 =
                            { mdl
                                | msg = Nothing
                                , token = Just authorization.token
                                , loginServer = Just server
                                , account = Just account
                                , instance = Nothing
                                , streams = []
                                , request =
                                    -- Fake the request
                                    Just <|
                                        Request.requestToRawRequest []
                                            serverInfo
                                            (AccountsRequest Request.GetVerifyCredentials)
                                , response = Just account.v
                                , entity = Just <| AccountEntity account
                            }
                                |> updateJsonTrees
                    in
                    mdl2
                        |> withCmds
                            [ cmd
                            , getAccountIdRelationships False mdl
                            , getLoggedInInstance model.server
                            ]

        ReceiveFetchAccount result ->
            case result of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok ( loginServer, token, account ) ->
                    let
                        serverInfo =
                            { server = loginServer
                            , token = Just token
                            }

                        request =
                            -- Fake the request
                            Request.requestToRawRequest []
                                serverInfo
                                (AccountsRequest Request.GetVerifyCredentials)

                        mdl =
                            { model
                                | msg = Nothing
                                , server = loginServer
                                , loginServer = Just loginServer
                                , instance = Nothing
                                , streams = []
                                , token = Just token
                                , account = Just account
                                , request = Just request
                                , response = Just account.v
                                , entity = Just <| AccountEntity account
                            }
                                |> updateJsonTrees
                    in
                    mdl
                        |> withCmds
                            [ getAccountIdRelationships False mdl
                            , getLoggedInInstance model.server
                            ]

        ReceiveInstance result ->
            case result of
                Err _ ->
                    -- We'll get lots of errors, for non-existant domains
                    model |> withNoCmd

                Ok response ->
                    case response.entity of
                        InstanceEntity instance ->
                            { model
                                | msg = Nothing
                                , request = Just response.rawRequest
                                , metadata = Just response.metadata
                                , response = Just instance.v
                                , entity = Just response.entity
                            }
                                |> updateJsonTrees
                                |> withNoCmd

                        _ ->
                            model |> withNoCmd

        ReceiveGetVerifyCredentials result ->
            case result of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok response ->
                    case response.entity of
                        AccountEntity account ->
                            let
                                mdl =
                                    { model
                                        | msg = Nothing
                                        , request = Just response.rawRequest
                                        , metadata = Just response.metadata
                                        , response = Just account.v
                                        , entity = Just response.entity
                                        , account = Just account
                                    }
                                        |> updateJsonTrees
                            in
                            mdl
                                |> withCmds
                                    [ getAccountIdRelationships False mdl
                                    , getLoggedInInstance model.server
                                    ]

                        _ ->
                            model |> withNoCmd

        SetInstance result ->
            let
                instance =
                    case result of
                        Err _ ->
                            Nothing

                        Ok response ->
                            case response.entity of
                                InstanceEntity inst ->
                                    Just inst

                                _ ->
                                    Nothing
            in
            { model | instance = instance }
                |> withNoCmd

        ReceiveAccountIdRelationships showResult result ->
            case result of
                Err _ ->
                    ( if showResult then
                        { model
                            | metadata = Nothing
                            , request = Nothing
                            , response = Nothing
                            , entity = Nothing
                            , selectedKeyPath = ""
                            , selectedKeyValue = ""
                        }

                      else
                        model
                    , Cmd.none
                    )

                Ok response ->
                    case response.entity of
                        RelationshipListEntity relationships ->
                            case
                                LE.find (\r -> r.id == model.accountId)
                                    relationships
                            of
                                Nothing ->
                                    model |> withNoCmd

                                Just relationship ->
                                    -- Maybe we should handle blocked_by
                                    let
                                        mdl =
                                            if not showResult then
                                                model

                                            else
                                                { model
                                                    | metadata =
                                                        Just response.metadata
                                                    , request =
                                                        Just response.rawRequest
                                                    , response =
                                                        Just relationship.v
                                                    , entity =
                                                        Just response.entity
                                                }
                                                    |> updateJsonTrees
                                    in
                                    mdl
                                        |> withNoCmd

                        _ ->
                            model |> withNoCmd

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok res ->
                    res

        SetLoginServer ->
            if model.server == "" then
                { model
                    | msg = Nothing
                    , loginServer = Nothing
                    , instance = Nothing
                    , streams = []
                    , request = Nothing
                    , response = Nothing
                    , entity = Nothing
                    , metadata = Nothing
                    , selectedKeyPath = ""
                    , selectedKeyValue = ""
                }
                    |> withNoCmd

            else
                let
                    mdl =
                        { model
                            | loginServer = Just model.server
                            , instance = Nothing
                            , streams = []
                            , token = Nothing
                            , account = Nothing
                        }
                            |> updateJsonTrees
                in
                sendRequest (InstanceRequest Request.GetInstance) mdl

        Login ->
            let
                url =
                    model.url

                sau =
                    { client_name = "mammudeck"
                    , server = model.server
                    , applicationUri =
                        { url
                            | fragment = Nothing
                            , query = Nothing
                        }
                            |> Url.toString
                    }
            in
            case Login.loginTask sau <| Dict.get model.server model.tokens of
                Redirect task ->
                    ( model, Task.attempt ReceiveRedirect task )

                FetchAccount task ->
                    ( model, Task.attempt ReceiveFetchAccount task )

        Logout ->
            case model.loginServer of
                Nothing ->
                    model |> withNoCmd

                Just server ->
                    { model
                        | server = ""
                        , loginServer = Nothing
                        , account = Nothing
                        , instance = Nothing
                        , tokens = Dict.remove server model.tokens
                        , token = Nothing
                        , request = Nothing
                        , response = Nothing
                        , entity = Nothing
                        , metadata = Nothing
                        , selectedKeyPath = ""
                        , selectedKeyValue = ""
                        , msg = Nothing
                        , streams = []
                    }
                        |> updateJsonTrees
                        |> withCmd (putToken server Nothing)

        ClearAll ->
            let
                mdl =
                    { model
                        | dialog = NoDialog
                        , tokens = Dict.empty
                        , server = ""
                        , loginServer = Nothing
                        , account = Nothing
                        , token = Nothing
                        , request = Nothing
                        , response = Nothing
                        , entity = Nothing
                        , selectedKeyPath = ""
                        , selectedKeyValue = ""
                        , metadata = Nothing
                        , msg = Nothing
                    }
                        |> updateJsonTrees
            in
            { mdl | savedModel = Just <| modelToSavedModel mdl }
                |> withCmd clear

        ReceiveResponse result ->
            receiveResponse result model


addStream : StreamType -> Model -> ( Model, Cmd Msg )
addStream streamType model =
    case model.instance of
        Nothing ->
            model |> withNoCmd

        Just instance ->
            case instance.urls of
                Nothing ->
                    { model | msg = Just "No streaming_api url." }
                        |> withNoCmd

                Just urls ->
                    let
                        index =
                            case List.head model.streams of
                                Nothing ->
                                    0

                                Just s ->
                                    s.index + 1

                        stream =
                            { index = index
                            , url =
                                Mastodon.WebSocket.streamUrl
                                    urls.streaming_api
                                    model.token
                                    streamType
                            , shown = True
                            , responses = []
                            , paused = False
                            }

                        message =
                            WebSocket.makeOpenWithKey (String.fromInt index) stream.url
                    in
                    { model
                        | streams = stream :: model.streams
                    }
                        |> withCmd (webSocketSend message)


getLoggedInInstance : String -> Cmd Msg
getLoggedInInstance server =
    Request.serverRequest (\id -> SetInstance)
        []
        { server = server, token = Nothing }
        ()
        (InstanceRequest Request.GetInstance)


updateStreams : Model -> Int -> Int -> (StreamResponse -> StreamResponse) -> Model
updateStreams model streamIndex responseIndex updater =
    case LE.find (streamPredicate streamIndex) model.streams of
        Nothing ->
            model

        Just stream ->
            case LE.find (streamResponsePredicate responseIndex) stream.responses of
                Nothing ->
                    model

                Just response ->
                    { model
                        | streams =
                            LE.setIf (streamPredicate streamIndex)
                                { stream
                                    | responses =
                                        LE.setIf (streamResponsePredicate responseIndex)
                                            (updater response)
                                            stream.responses
                                }
                                model.streams
                    }


taggedValueToString : TaggedValue -> String
taggedValueToString taggedValue =
    case taggedValue of
        TString string ->
            string

        TFloat float ->
            String.fromFloat float

        TBool bool ->
            if bool then
                "true"

            else
                "false"

        TNull ->
            "null"

        _ ->
            ""


findKeyPath : String -> JsonTree.Node -> Maybe JsonTree.TaggedValue
findKeyPath keyPath node =
    if keyPath == node.keyPath then
        Just node.value

    else
        let
            loop nodes =
                case nodes of
                    [] ->
                        Nothing

                    first :: rest ->
                        case findKeyPath keyPath first of
                            Nothing ->
                                loop rest

                            res ->
                                res
        in
        case node.value of
            TList nodes ->
                loop nodes

            TDict dict ->
                Dict.values dict
                    |> loop

            _ ->
                Nothing


ifNotEqual : a -> a -> Maybe a
ifNotEqual new old =
    if new == old then
        Nothing

    else
        Just new


getAccountIdRelationships : Bool -> Model -> Cmd Msg
getAccountIdRelationships showResult model =
    case model.account of
        Nothing ->
            Cmd.none

        Just account ->
            case model.loginServer of
                Nothing ->
                    Cmd.none

                Just server ->
                    case model.accountId of
                        "" ->
                            Cmd.none

                        accountId ->
                            Request.serverRequest ReceiveAccountIdRelationships
                                []
                                { server = server
                                , token = model.token
                                }
                                showResult
                                (AccountsRequest <|
                                    Request.GetRelationships
                                        { ids = [ accountId ] }
                                )


getUsername : Model -> String
getUsername model =
    let
        username =
            model.username
    in
    if username /= "" then
        username

    else
        case model.account of
            Just account ->
                account.username

            Nothing ->
                ""


getAccountId : Model -> String
getAccountId model =
    let
        id =
            model.accountId
    in
    if id /= "" then
        id

    else
        case model.account of
            Just account ->
                account.id

            Nothing ->
                ""


receiveResponse : Result Error Response -> Model -> ( Model, Cmd Msg )
receiveResponse result model =
    case result of
        Err err ->
            let
                threeStrikes =
                    ( Nothing, Nothing, Nothing )

                ( msg, ( response, entity, metadata ) ) =
                    case err of
                        BadUrl url ->
                            ( "BadUrl: " ++ url, threeStrikes )

                        Timeout ->
                            ( "Timeout", threeStrikes )

                        NetworkError ->
                            ( "Network Error", threeStrikes )

                        BadStatus meta status ->
                            ( "Bad status: " ++ status, ( Nothing, Nothing, Just meta ) )

                        BadBody meta jderr json ->
                            let
                                res =
                                    case JD.decodeString JD.value json of
                                        Err _ ->
                                            Nothing

                                        Ok v ->
                                            Just v

                                m =
                                    "BadBody: " ++ decodeErrorToString jderr
                            in
                            ( m, ( res, Nothing, Just meta ) )
            in
            { model
                | msg = Just msg
                , response = response
                , entity = entity
                , metadata = metadata
                , selectedKeyPath = ""
                , selectedKeyValue = ""
            }
                |> updateJsonTrees
                |> withNoCmd

        Ok response ->
            let
                mdl =
                    applyResponseSideEffects response model
            in
            { mdl
                | msg = Nothing
                , metadata = Just response.metadata
                , response = Just <| ED.entityValue (Debug.log "entity" response.entity)
                , entity = Just response.entity
            }
                |> updateJsonTrees
                |> withNoCmd


decodeErrorToString : JD.Error -> String
decodeErrorToString error =
    case error of
        JD.Field field err ->
            "Error on field \"" ++ field ++ "\": " ++ decodeErrorToString err

        JD.Index idx err ->
            "Error on index " ++ String.fromInt idx ++ "\": " ++ decodeErrorToString err

        JD.OneOf errors ->
            case errors of
                [] ->
                    "OneOf []"

                err :: _ ->
                    decodeErrorToString err

        JD.Failure fail value ->
            -- TODO: encode some part of `value`.
            fail


applyResponseSideEffects : Response -> Model -> Model
applyResponseSideEffects response model =
    case response.request of
        InstanceRequest Request.GetInstance ->
            case response.entity of
                InstanceEntity instance ->
                    { model | instance = Just instance }

                _ ->
                    model

        _ ->
            model


splitMediaIds : String -> List String
splitMediaIds string =
    let
        s =
            String.trim string
    in
    if s == "" then
        []

    else
        String.split "," s
            |> List.map String.trim


sendRequest : Request -> Model -> ( Model, Cmd Msg )
sendRequest request model =
    case model.loginServer of
        Nothing ->
            model |> withNoCmd

        Just server ->
            let
                rawRequest =
                    Request.requestToRawRequest []
                        { server = server
                        , token = model.token
                        }
                        request
            in
            { model
                | request = Just rawRequest
                , response = Nothing
                , entity = Nothing
                , metadata = Nothing
                , selectedKeyPath = ""
                , selectedKeyValue = ""
            }
                |> withCmd
                    (Request.rawRequestToCmd ReceiveResponse rawRequest)


saveAuthorization : String -> Authorization -> Model -> ( Model, Cmd Msg )
saveAuthorization server authorization model =
    let
        tokens =
            model.tokens
    in
    { model
        | tokens =
            Dict.insert server
                authorization.token
                tokens
    }
        |> withCmd (putToken server <| Just authorization.token)


serverOption : String -> String -> Html Msg
serverOption currentServer server =
    option
        [ value server
        , selected <| server == currentServer
        ]
        [ text server ]


serverSelect : Model -> Html Msg
serverSelect model =
    let
        currentServer =
            case model.loginServer of
                Nothing ->
                    ""

                Just server ->
                    server
    in
    select [ onInput SetServer ]
        (option [ value "" ]
            [ text "-- select a server --" ]
            :: (List.map (serverOption currentServer) <| Dict.keys model.tokens)
        )


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


type alias StyleProperties =
    { backgroundColor : String
    , color : String
    }


type Style
    = DarkStyle
    | LightStyle


styles :
    { dark : StyleProperties
    , light : StyleProperties
    }
styles =
    { dark =
        { backgroundColor = "#222"
        , color = "#eee"
        }
    , light =
        { backgroundColor = "white"
        , color = "black"
        }
    }


getStyle : Style -> StyleProperties
getStyle style =
    case style of
        DarkStyle ->
            styles.dark

        LightStyle ->
            styles.light


link : String -> String -> Html Msg
link label url =
    a
        [ href url
        , blankTarget
        ]
        [ text label ]


blankTarget : Attribute msg
blankTarget =
    target "_blank"


{-| The Material Design CSS puts paragraph spacing BELOW the paragraph.

Use this to make a paragraph worth of vertical white space.

-}
pspace : Html msg
pspace =
    p [] [ text "" ]


enabledButton : Bool -> Msg -> String -> Html Msg
enabledButton enabled msg label =
    Html.button
        [ onClick msg
        , disabled <| not enabled
        ]
        [ b label ]


button : Msg -> String -> Html Msg
button =
    enabledButton True


separator : String -> Html msg
separator width =
    hr
        [ style "width" width
        , style "margin" "0 auto"
        ]
        []


removeStreamUrlBearer : String -> String
removeStreamUrlBearer string =
    case Url.fromString <| String.replace "wss:" "https:" string of
        Nothing ->
            string

        Just url ->
            case url.query of
                Nothing ->
                    string

                Just query ->
                    if String.startsWith "access_token=" query then
                        let
                            indices =
                                String.indices "&" query

                            q =
                                case indices of
                                    [] ->
                                        ""

                                    index :: _ ->
                                        String.dropLeft (index + 1) query
                        in
                        Url.toString
                            { url
                                | query = Just q
                            }
                            |> String.replace "https:" "wss:"

                    else
                        string


renderStream : Model -> Stream -> Html Msg
renderStream model stream =
    p []
        [ separator "100%"
        , button (ToggleStreamShown stream.index) <|
            if stream.shown then
                "-"

            else
                "+"
        , text " "
        , button (ToggleStreamPaused stream.index) <|
            if stream.paused then
                "On"

            else
                "Off"
        , text " "
        , button (RemoveStream stream.index) "X"
        , text " "
        , text <| removeStreamUrlBearer stream.url
        , br
        , if not stream.shown then
            text ""

          else
            span []
                (List.map (renderStreamResponse model.style stream.index) stream.responses
                    |> List.intersperse (separator "70%")
                )
        ]


renderStreamResponse : Style -> Int -> StreamResponse -> Html Msg
renderStreamResponse style streamIndex { index, response, tree, treeState } =
    let
        config =
            jsonTreeConfig style (SetStreamResponseState streamIndex index)
    in
    case tree of
        Nothing ->
            text response

        Just root ->
            JsonTree.view root config treeState


view : Model -> Document Msg
view model =
    let
        { backgroundColor, color } =
            getStyle model.style
    in
    { title = "Mastodon WebSocket"
    , body =
        [ renderDialog model
        , div
            [ style "background-color" backgroundColor
            , style "padding" "1em 0 0 0"
            , style "margin" "0"
            , style "width" "auto"
            ]
            [ div
                [ style "color" color
                , style "background-color" backgroundColor
                , style "padding" "1em 3em 1em 3em"
                , style "max-width" "fill-available"
                , style "width" "40em"
                , style "margin" "auto"
                ]
                [ div []
                    [ h2 [] [ text "Mastodon WebSocket" ]
                    , case model.loginServer of
                        Nothing ->
                            text ""

                        Just server ->
                            p []
                                [ b "Use API for: "
                                , link server <| "https://" ++ server
                                , text " "
                                , button Logout "Logout"
                                , br
                                , case model.account of
                                    Nothing ->
                                        text ""

                                    Just account ->
                                        span []
                                            [ b "Username: "
                                            , text account.display_name
                                            , text " ("
                                            , link account.username account.url
                                            , text ")"
                                            ]
                                ]
                    , p []
                        [ loginSelectedUI model
                        , if model.instance == Nothing then
                            text ""

                          else
                            addStreamUI model
                        ]
                    , p [ style "color" "red" ]
                        [ Maybe.withDefault "" model.msg |> text ]
                    , p [] <|
                        List.map (renderStream model) model.streams
                    , p []
                        [ checkBox ToggleShowJsonTree model.showJsonTree "show trees"
                        , if model.showJsonTree then
                            text ""

                          else
                            span []
                                [ text " "
                                , checkBox TogglePrettify model.prettify "prettify"
                                ]
                        , text " "
                        , button ClearSentReceived "Clear"
                        ]
                    , p [] [ b "Sent:" ]
                    , pre []
                        [ case model.request of
                            Nothing ->
                                text ""

                            Just request ->
                                span []
                                    [ text request.method
                                    , text " "
                                    , text request.url
                                    , case request.jsonBody of
                                        Nothing ->
                                            text ""

                                        Just value ->
                                            pre []
                                                [ text <|
                                                    encodeWrap model.prettify value
                                                ]
                                    ]
                        ]
                    , p [] <|
                        if model.showMetadata then
                            [ b "Headers: "
                            , button ToggleShowMetadata "Hide"
                            ]

                        else
                            [ b "Headers "
                            , button ToggleShowMetadata "Show"
                            ]
                    , if not model.showMetadata then
                        text ""

                      else
                        case model.metadata of
                            Nothing ->
                                text ""

                            Just metadata ->
                                p []
                                    [ renderHeaders model.prettify color metadata ]
                    , p [] <|
                        if model.showReceived then
                            [ b "Received:"
                            , button ToggleShowReceived "Hide"
                            ]

                        else
                            [ b "Received "
                            , button ToggleShowReceived "Show"
                            ]
                    , renderJson ResponseJson
                        model
                        model.showReceived
                        model.response
                        Nothing
                    , p [] <|
                        if model.showEntity then
                            [ b "Decoded:"
                            , button ToggleShowEntity "Hide"
                            ]

                        else
                            [ b "Decoded "
                            , button ToggleShowEntity "Show"
                            ]
                    , renderJson DecodedJson
                        model
                        model.showEntity
                        Nothing
                        model.entity
                    , div []
                        [ help model ]
                    , br
                    , p []
                        [ button
                            (SetDialog <|
                                ConfirmDialog
                                    "Do you really want to erase everything?"
                                    "Erase"
                                    ClearAll
                            )
                            "Clear All Persistent State"
                        ]
                    , br
                    , p
                        [ onClick ToggleStyle
                        , style "cursor" "default"
                        ]
                        [ input
                            [ type_ "checkbox"
                            , checked <| model.style == DarkStyle
                            ]
                            []
                        , b "Dark Mode"
                        ]
                    , p []
                        [ text <| "Copyright " ++ special.copyright ++ " 2019, Bill St. Clair"
                        , br
                        , link "@billstclair@impeccable.social"
                            "https://impeccable.social/billstclair"
                        , br
                        , text "API Docs: "
                        , link "docs.joinmastodon.org"
                            "https://docs.joinmastodon.org/api/streaming/"
                        , br
                        , text "Source code: "
                        , link "GitHub"
                            "https://github.com/billstclair/elm-mastodon-websocket"
                        ]
                    ]
                ]
            ]
        ]
    }


renderJson : WhichJson -> Model -> Bool -> Maybe Value -> Maybe Entity -> Html Msg
renderJson whichJson model enabled value entity =
    if not enabled then
        text ""

    else
        let
            mv =
                case value of
                    Just val ->
                        Just val

                    Nothing ->
                        case entity of
                            Nothing ->
                                Nothing

                            Just e ->
                                Just <| ED.encodeEntity e
        in
        case mv of
            Nothing ->
                text ""

            Just v ->
                if model.showJsonTree then
                    renderJsonTree whichJson model v

                else
                    pre []
                        [ text <| encodeWrap model.prettify v ]


type WhichJson
    = ResponseJson
    | DecodedJson
    | StreamJson { streamIndex : Int, response : StreamResponse }


jsonTreeColors =
    let
        default =
            JsonTree.defaultColors
    in
    { light = default
    , dark = { default | number = "turquoise" }
    }


jsonTreeConfig : Style -> (JsonTree.State -> Msg) -> JsonTree.Config Msg
jsonTreeConfig style setter =
    { colors =
        if style == DarkStyle then
            jsonTreeColors.dark

        else
            jsonTreeColors.light

    -- Should copy the text to a <TextArea> on select.
    , onSelect = Nothing
    , toMsg = setter
    }


renderJsonTree : WhichJson -> Model -> Value -> Html Msg
renderJsonTree whichJson model value =
    let
        ( setter, maybeTree, state ) =
            case whichJson of
                ResponseJson ->
                    ( SetResponseState
                    , Result.toMaybe model.responseTree
                    , model.responseState
                    )

                DecodedJson ->
                    ( SetEntityState
                    , Result.toMaybe model.entityTree
                    , model.entityState
                    )

                StreamJson { streamIndex, response } ->
                    ( SetStreamState streamIndex response.index
                    , response.tree
                    , response.treeState
                    )

        config =
            jsonTreeConfig model.style setter
    in
    case maybeTree of
        Nothing ->
            text ""

        Just root ->
            span []
                [ button (ExpandAll whichJson) "Expand All"
                , text " "
                , button (CollapseAll whichJson) "Collapse All"
                , case root.value of
                    TList nodes ->
                        span []
                            [ br
                            , b "length: "
                            , text (String.fromInt <| List.length nodes)
                            ]

                    _ ->
                        text ""
                , br
                , JsonTree.view root config state
                ]


maybeLabeledTextInput : Maybe String -> Int -> (String -> Msg) -> String -> Html Msg
maybeLabeledTextInput label inputSize wrapper string =
    let
        inputBox =
            input
                [ size inputSize
                , onInput wrapper
                , value string
                ]
                []
    in
    case label of
        Nothing ->
            inputBox

        Just lab ->
            span []
                [ b lab
                , inputBox
                ]


textInput : String -> Int -> (String -> Msg) -> String -> Html Msg
textInput label =
    maybeLabeledTextInput (Just label)


unlabeledTextInput : Int -> (String -> Msg) -> String -> Html Msg
unlabeledTextInput =
    maybeLabeledTextInput Nothing


checkBox : Msg -> Bool -> String -> Html Msg
checkBox msg isChecked label =
    span
        [ onClick msg
        , style "cursor" "default"
        ]
        [ input
            [ type_ "checkbox"
            , checked isChecked
            ]
            []
        , b label
        ]


loginSelectedUI : Model -> Html Msg
loginSelectedUI model =
    p []
        [ b "server: "
        , input
            [ size 30
            , onInput SetServer
            , value model.server
            , placeholder "mastodon.social"
            ]
            []
        , text " "
        , serverSelect model
        , br
        , Html.button
            [ onClick Login
            , disabled <| model.server == ""
            ]
            [ b "Login" ]
        , text " "
        , button SetLoginServer "Set Server"
        ]


addStreamUI : Model -> Html Msg
addStreamUI model =
    let
        enable =
            model.hashOrId /= ""
    in
    p []
        [ b "Add Stream"
        , br
        , button (AddStream UserStream) "user"
        , text " "
        , button (AddStream PublicStream) "public"
        , text " "
        , button (AddStream LocalStream) "public:local"
        , text " "
        , button (AddStream ProStream) "pro"
        , text " "
        , button (AddStream DirectStream) "direct"
        , br
        , textInput "hashtag or id: " 20 SetHashOrId model.hashOrId
        , br
        , enabledButton enable (AddHashOrIdStream PublicHashtagStream) "hashtag"
        , text " "
        , enabledButton enable (AddHashOrIdStream LocalHashtagStream) "hashtag:local"
        , text " "
        , enabledButton enable (AddHashOrIdStream ListStream) "list"
        , text " "
        , enabledButton enable (AddHashOrIdStream GroupStream) "group"
        ]


renderHeaders : Bool -> String -> Http.Metadata -> Html Msg
renderHeaders prettify color metadata =
    let
        fold k v res =
            let
                vline =
                    if not prettify then
                        v

                    else
                        wrapJsonLine wrapColumns v
                            |> String.join "\n"
            in
            tr []
                [ td [] [ pre [] [ text k ] ]
                , td [] [ pre [] [ text vline ] ]
                ]
                :: res
    in
    span []
        [ b "status code: "
        , text <| String.fromInt metadata.statusCode
        , br
        , b "status text: "
        , text metadata.statusText
        , Dict.foldr fold [] metadata.headers
            |> table
                [ style "color" color
                , style "font-size" "14px"
                ]
        ]


cancelButtonId : String
cancelButtonId =
    "cancelButton"


renderDialog : Model -> Html Msg
renderDialog model =
    let
        ( content, okButtonText, msg ) =
            case model.dialog of
                NoDialog ->
                    ( "You should never see this.", "OK", Noop )

                ConfirmDialog cont ok m ->
                    ( cont, ok, m )
    in
    Dialog.render
        { styles = [ ( "width", "40%" ) ]
        , title = "Confirm"
        , content = [ text content ]
        , actionBar =
            [ Html.button
                [ onClick <| SetDialog NoDialog
                , id cancelButtonId
                ]
                [ b "Cancel" ]
            , text <| String.repeat 4 special.nbsp
            , button msg okButtonText
            ]
        }
        (model.dialog /= NoDialog)


help : Model -> Html Msg
help model =
    Markdown.toHtml [] <|
        """
**Help**

Type a server name, e.g. "mastodon.social" in the "server" field. If it is a valid Mastodon server, its instance entity will appear. Click "Login" to login as a registered user, or "Set Server" to use the server without logging in.

Click "Logout" to remove the current server's remembered access token.

Click on one of the choices below "Add Stream" to add that type of WebSocket stream. The "hashtag or id" is necessary for the "hashtag", "hashtag:local", "list", and "group" streams.

A section will appear for the stream, showing the URL to connect, with the access token removed, and filling up as events come in.

Click "-" to hide a stream's contents and "+" to show it again.

Click "Off" to stop updating a stream's contents, and "On" to start again.

Click "X" to remove a stream and close its websocket connection.

Click "Clear All Persistent State" below this help to remove the remembered access tokens and preferences.

Toggle the "Dark Mode" checkbox to switch appearance.
            """


convertJsonNewlines : String -> String
convertJsonNewlines json =
    String.replace "\\r" "" json
        |> String.replace "\\n" "\n"


wrapJsonLine : Int -> String -> List String
wrapJsonLine width line =
    let
        body =
            String.trimLeft line

        indentN =
            String.length line - String.length body + 2

        initialIndent =
            String.repeat (indentN - 2) " "

        indent =
            String.repeat indentN " "

        wrapped =
            convertJsonNewlines body
                |> String.split "\n"
                |> List.map (SE.softWrap <| max 20 (width - indentN))
                |> String.join "\n"

        lines =
            String.split "\n" wrapped
    in
    case lines of
        [] ->
            []

        first :: rest ->
            (initialIndent ++ first)
                :: List.map ((++) indent) rest


wrapJsonLines : Int -> String -> String
wrapJsonLines width string =
    String.split "\n" string
        |> List.concatMap (wrapJsonLine width)
        |> String.join "\n"


wrapColumns : Int
wrapColumns =
    80


encodeWrap : Bool -> Value -> String
encodeWrap prettify value =
    JE.encode 2 value
        |> (if prettify then
                wrapJsonLines wrapColumns

            else
                identity
           )



---
--- Persistence
---


type alias SavedModel =
    { loginServer : Maybe String
    , token : Maybe String
    , server : String
    , showJsonTree : Bool
    , prettify : Bool
    , style : Style
    , showMetadata : Bool
    , showReceived : Bool
    , showEntity : Bool
    , maxStreamResponses : Int
    }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { loginServer = model.loginServer
    , token = model.token
    , server = model.server
    , showJsonTree = model.showJsonTree
    , prettify = model.prettify
    , style = model.style
    , showMetadata = model.showMetadata
    , showReceived = model.showReceived
    , showEntity = model.showEntity
    , maxStreamResponses = model.maxStreamResponses
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    { model
        | loginServer = savedModel.loginServer
        , token = savedModel.token
        , server = savedModel.server
        , showJsonTree = savedModel.showJsonTree
        , prettify = savedModel.prettify
        , style = savedModel.style
        , showMetadata = savedModel.showMetadata
        , showReceived = savedModel.showReceived
        , showEntity = savedModel.showEntity
        , maxStreamResponses = savedModel.maxStreamResponses
    }


encodeSavedModel : SavedModel -> Value
encodeSavedModel savedModel =
    JE.object
        [ ( "loginServer", ED.encodeMaybe JE.string savedModel.loginServer )
        , ( "token", ED.encodeMaybe JE.string savedModel.token )
        , ( "server", JE.string savedModel.server )
        , ( "showJsonTree", JE.bool savedModel.showJsonTree )
        , ( "prettify", JE.bool savedModel.prettify )
        , ( "darkstyle", JE.bool <| savedModel.style == DarkStyle )
        , ( "showMetadata", JE.bool savedModel.showMetadata )
        , ( "showReceived", JE.bool savedModel.showReceived )
        , ( "showEntity", JE.bool savedModel.showEntity )
        , ( "maxStreamResponses", JE.int savedModel.maxStreamResponses )
        ]


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> optional "loginServer" (JD.nullable JD.string) Nothing
        |> optional "token" (JD.nullable JD.string) Nothing
        |> required "server" JD.string
        |> optional "showJsonTree" JD.bool True
        |> optional "prettify" JD.bool True
        |> optional "darkstyle"
            (JD.bool
                |> JD.andThen
                    (\x ->
                        JD.succeed <|
                            if x then
                                DarkStyle

                            else
                                LightStyle
                    )
            )
            LightStyle
        |> optional "showMetadata" JD.bool False
        |> optional "showReceived" JD.bool True
        |> optional "showEntity" JD.bool False
        |> optional "maxStreamResponses" JD.int 5


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


get : String -> Cmd Msg
get key =
    localStorageSend (LocalStorage.get <| Debug.log "get" key)


getLabeled : String -> String -> Cmd Msg
getLabeled label key =
    localStorageSend
        (LocalStorage.getLabeled label <|
            Debug.log ("getLabeled " ++ label) key
        )


listKeysLabeled : String -> String -> Cmd Msg
listKeysLabeled label prefix =
    localStorageSend (LocalStorage.listKeysLabeled label prefix)


tokenStorageKey : String -> String
tokenStorageKey server =
    pk.token ++ "." ++ server


tokenStorageKeyServer : String -> String
tokenStorageKeyServer key =
    String.dropLeft (String.length pk.token + 1) key


getToken : String -> Cmd Msg
getToken server =
    getLabeled pk.token <| tokenStorageKey server


putToken : String -> Maybe String -> Cmd Msg
putToken server token =
    put (tokenStorageKey server) <|
        case token of
            Nothing ->
                Nothing

            Just tok ->
                Just <| JE.string tok


clear : Cmd Msg
clear =
    localStorageSend (LocalStorage.clear "")


localStoragePrefix : String
localStoragePrefix =
    "mammudeck"


port cmdPort : Value -> Cmd msg


port subPort : (Value -> msg) -> Sub msg


initialFunnelState : State Msg
initialFunnelState =
    PortFunnels.initialState
        { localStoragePrefix = localStoragePrefix
        , cmdPort = cmdPort
        , subPort = subPort
        }


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send cmdPort
        message
        initialFunnelState.storage


webSocketSend : WebSocket.Message -> Cmd Msg
webSocketSend message =
    WebSocket.send cmdPort <|
        Debug.log "webSocketSend" message


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : State Msg -> String -> model -> (Value -> Cmd Msg)
getCmdPort state moduleName model =
    cmdPort


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler
        , WebSocketHandler socketHandler
        ]
        getCmdPort


{-| Persistent storage keys
-}
pk =
    { model = "websocket-model"
    , token = "token"
    }



---
--- Special characters
---


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


special =
    { nbsp = stringFromCode 160 -- \u00A0
    , copyright = stringFromCode 169 -- \u00A9
    }
