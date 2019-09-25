----------------------------------------------------------------------
--
-- PortFunnels.elm
-- Most of the support needed for a PortFunnel application
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Mastodon.PortFunnels exposing
    ( FunnelDict, Handler(..), State
    , getCmdPort, subscriptions, initialState, makeFunnelDict
    , processValue
    )

{-| Port Funnels

This is the interface to the JavaScript code for websockets and localStorage.

Your application needs to provide the actual ports, or simulators. See the
example for details, and how to initialize the JavaScript code for the ports.


# Types

@docs FunnelDict, Handler, State


# Initialization

@docs getCmdPort, subscriptions, initialState, makeFunnelDict


# Processing

@docs processValue

-}

import Dict exposing (Dict)
import Json.Encode as JE exposing (Value)
import PortFunnel
    exposing
        ( FunnelSpec
        , GenericMessage
        , ModuleDesc
        , StateAccessors
        )
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.WebSocket as WebSocket


{-| Add a property to this type for each funnel module you use.
-}
type alias State msg =
    { storage : LocalStorage.State
    , websocket : WebSocket.State
    , cmdPort : Value -> Cmd msg
    , subPort : (Value -> msg) -> Sub msg
    }


{-| Create the initial state record.
-}
initialState :
    { localStoragePrefix : String
    , cmdPort : Value -> Cmd msg
    , subPort : (Value -> msg) -> Sub msg
    }
    -> State msg
initialState { localStoragePrefix, cmdPort, subPort } =
    { storage = LocalStorage.initialState localStoragePrefix
    , websocket = WebSocket.initialState
    , cmdPort = cmdPort
    , subPort = subPort
    }


{-| Make a `StateAccessors` instance for each funnel module.
-}
localStorageAccessors : StateAccessors (State msg) LocalStorage.State
localStorageAccessors =
    StateAccessors .storage (\substate state -> { state | storage = substate })


websocketAccessors : StateAccessors (State msg) WebSocket.State
websocketAccessors =
    StateAccessors .websocket (\substate state -> { state | websocket = substate })


{-| A `Funnel` tags a module-specific `FunnelSpec`.
-}
type Funnel model msg
    = LocalStorageFunnel (FunnelSpec (State msg) LocalStorage.State LocalStorage.Message LocalStorage.Response model msg)
    | WebSocketFunnel (FunnelSpec (State msg) WebSocket.State WebSocket.Message WebSocket.Response model msg)


{-| A `Handler` tags a function to handle responses from one funnel module.

Add a tag in this type for each funnel module you use.

-}
type Handler model msg
    = LocalStorageHandler (LocalStorage.Response -> State msg -> model -> ( model, Cmd msg ))
    | WebSocketHandler (WebSocket.Response -> State msg -> model -> ( model, Cmd msg ))


{-| This packages up everything necessary to dispatch for each module.

Add a clause for each funnel module you use.

-}
handlerToFunnel : Handler model msg -> ( String, Funnel model msg )
handlerToFunnel handler =
    case handler of
        LocalStorageHandler localStorageHandler ->
            ( LocalStorage.moduleName
            , FunnelSpec localStorageAccessors LocalStorage.moduleDesc LocalStorage.commander localStorageHandler
                |> LocalStorageFunnel
            )

        WebSocketHandler websocketHandler ->
            ( WebSocket.moduleName
            , FunnelSpec websocketAccessors WebSocket.moduleDesc WebSocket.commander websocketHandler
                |> WebSocketFunnel
            )


{-| Add a tuple to this list for each funnel module you use.
-}
simulatedPortDict : Dict String ((Value -> msg) -> Value -> Cmd msg)
simulatedPortDict =
    Dict.fromList
        [ ( LocalStorage.moduleName, LocalStorage.makeSimulatedCmdPort )
        , ( WebSocket.moduleName, WebSocket.makeSimulatedCmdPort )
        ]


{-| This is called from `AppFunnel.processValue`.

It unboxes the `Funnel` arg, and calls `PortFunnel.appProcess`.

-}
appTrampoline : (State msg -> String -> model -> (Value -> Cmd msg)) -> GenericMessage -> Funnel model msg -> State msg -> model -> Result String ( model, Cmd msg )
appTrampoline portGetter genericMessage funnel state model =
    -- Dispatch on the `Funnel` tag.
    -- This example has only one possibility.
    case funnel of
        LocalStorageFunnel appFunnel ->
            PortFunnel.appProcess (portGetter state LocalStorage.moduleName model)
                genericMessage
                appFunnel
                state
                model

        WebSocketFunnel appFunnel ->
            PortFunnel.appProcess (portGetter state WebSocket.moduleName model)
                genericMessage
                appFunnel
                state
                model


{-| Turn the `moduleName` inside a `GenericMessage` into the output port.

    getCmdPort tagger moduleName useSimulator

`tagger` is the same `Msg` that processes input from the subscription port.

`moduleName` will be ignored if `useSimulator` is `False`.

-}
getCmdPort : State msg -> (Value -> msg) -> String -> Bool -> (Value -> Cmd msg)
getCmdPort state tagger moduleName useSimulator =
    if not useSimulator then
        state.cmdPort

    else
        case Dict.get moduleName simulatedPortDict of
            Just makeSimulatedCmdPort ->
                makeSimulatedCmdPort tagger

            Nothing ->
                state.cmdPort


{-| Create a subscription for the `subPort`, given a Msg wrapper.
-}
subscriptions : State msg -> (Value -> msg) -> model -> Sub msg
subscriptions state process model =
    state.subPort process


{-| A `Dict` that maps a module name to a concretized `FunnelSpec`.

Create one with `makeFunnelDict`. Pass it to `processValue`.

-}
type alias FunnelDict model msg =
    ( Dict String (Funnel model msg), State msg -> String -> model -> (Value -> Cmd msg) )


{-| Make a `Dict` mapping `moduleName` to tagged concrete `FunnelSpec`.

The `Handler` list is a list of all of your handlers. E.g. for this example, it would be:

    makeFunnelDict
        [ LocalStorageHandler localStorageHandler
        ]
        getCmdPort

Where `echoHandler` and `addXYHandler` are functions in your main application module to handle responses.

-}
makeFunnelDict : List (Handler model msg) -> (State msg -> String -> model -> (Value -> Cmd msg)) -> FunnelDict model msg
makeFunnelDict handlers portGetter =
    ( List.map handlerToFunnel handlers |> Dict.fromList
    , portGetter
    )


{-| Process a value coming in through the `subPort`.

The `FunnelDict` is the result of calling `makeFunnelDict`.

-}
processValue : FunnelDict model msg -> Value -> State msg -> model -> Result String ( model, Cmd msg )
processValue ( funnelDict, portGetter ) value state model =
    PortFunnel.processValue funnelDict (appTrampoline portGetter) value state model
