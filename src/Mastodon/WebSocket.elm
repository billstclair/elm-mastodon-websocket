----------------------------------------------------------------------
--
-- WebSocket.elm
-- An Elm client for the WebSocket API of the Mastodon social networking system.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Mastodon.WebSocket exposing
    ( StreamType(..), Event(..)
    , streamUrl, open, close, decodeEvent, eventDecoder
    )

{-| The WebSocket API for the Mastodon Social Network.


# Types

@docs StreamType, Event


# Functions

@docs streamUrl, open, close, decodeEvent, eventDecoder

-}

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Mastodon.EncodeDecode as ED
import Mastodon.Entity exposing (Notification, Status)
import Mastodon.PortFunnels exposing (State)
import PortFunnel.WebSocket
import Url.Builder as Builder exposing (QueryParameter)


{-| Stream types.

Use `streamingUrl` to convert one of these into a url string.

The `String` arg to `PublicHashtagStream` and `LocalHashtagStream` is a hashtag.

The `String` arg to `ListStream` is a list ID.

The `String` arg to `GroupStream` is a group ID.
Groups are currently supported only by the Gab server code.

-}
type StreamType
    = UserStream
    | PublicStream
    | LocalStream
    | PublicHashtagStream String
    | LocalHashtagStream String
    | ListStream String
    | DirectStream
    | GroupStream String


{-| Convert a streaming API (e.g. "rss://mastodon.social"), access token, and stream type into a URL for the WebSocket connection.

    streamUrl streaming_api accessToken streamType

If `accessToken` is `Nothing`, will attempt to connect without authentication. This works for the following `StreamType`s: [TODO]

-}
streamUrl : String -> Maybe String -> StreamType -> String
streamUrl streaming_api accessToken streamType =
    Builder.crossOrigin
        streaming_api
        [ "api", "v1", "streaming" ]
        (streamTypeToQueryParameters accessToken streamType)


streamTypeToQueryParameters : Maybe String -> StreamType -> List QueryParameter
streamTypeToQueryParameters accessToken streamType =
    List.concat
        [ case accessToken of
            Nothing ->
                []

            Just token ->
                let
                    bearer =
                        "Bearer "

                    tok =
                        if String.startsWith bearer token then
                            String.dropLeft (String.length bearer) token

                        else
                            token
                in
                [ Builder.string "access_token" tok ]
        , case streamType of
            UserStream ->
                [ Builder.string "stream" "user" ]

            PublicStream ->
                [ Builder.string "stream" "public" ]

            LocalStream ->
                [ Builder.string "stream" "public:local" ]

            PublicHashtagStream hashtag ->
                [ Builder.string "stream" "hashtag"
                , Builder.string "tag" hashtag
                ]

            LocalHashtagStream hashtag ->
                [ Builder.string "stream" "hashtag:local"
                , Builder.string "tag" hashtag
                ]

            ListStream id ->
                [ Builder.string "stream" "list"
                , Builder.string "list" id
                ]

            DirectStream ->
                [ Builder.string "stream" "direct" ]

            GroupStream id ->
                [ Builder.string "stream" "group"
                , Builder.string "group" id
                ]
        ]


{-| Open a WebSocket connection.

    open state key url

`state` is initially the result of `WebSocket.PortFunnels.initialState`.

`key` is a unique key string.

`url` is the result of a call to `streamUrl`.

-}
open : State msg -> String -> String -> Cmd msg
open state key url =
    PortFunnel.WebSocket.makeOpenWithKey key url
        |> PortFunnel.WebSocket.send state.cmdPort


{-| Close a socket opened with `open`.

    close state key

-}
close : State msg -> String -> Cmd msg
close state key =
    PortFunnel.WebSocket.makeClose key
        |> PortFunnel.WebSocket.send state.cmdPort


{-| An event received over the websocket.

`NoEvent`, `UpdateEvent`, `NotificationEvent`, `DeleteEvent`, and
`FiltersChangedEvent` come over the wire as specified at
<https://docs.joinmastodon.org/api/streaming>.

`ConnectedEvent` is delivered as soon as the initial connection is made, right
after your send a `PortFunnels.WebSocket.makeOpen` message over the socket.

`ReconnectedEvent` is delivered if the connection is lost and auto-reconnected.

`ClosedEvent` is delivered if the reconnection fails, or you explicitly send
a `PortFunnels.WebSocket.makeClose` message.

-}
type Event
    = NoEvent
    | UpdateEvent Status
    | NotificationEvent Notification
    | DeleteEvent String
    | FiltersChangedEvent
      -- The string is the description from `PortFunnel.WebSocket.ConnectedResponse`
      -- or `PortFunnel.WebSocket.ReconnectedResponse`.
    | ConnectedEvent String
    | ReconnectedEvent String
      -- A message that can't be decoded
    | UnknownEvent String
      -- When the socket connection is closed
    | ClosedEvent String


{-| Decode a string from the WebSocket stream.
-}
decodeEvent : String -> Result JD.Error Event
decodeEvent string =
    if "" == string || ":" == String.left 1 string then
        Ok NoEvent

    else
        JD.decodeString eventDecoder string


{-| The JSON decoder for a non-colon event.

You'll usually use `decodeEvent` on the string that comes over the WebSocket."

-}
eventDecoder : Decoder Event
eventDecoder =
    JD.value
        |> JD.andThen
            (\value ->
                case JD.decodeValue (JD.field "event" JD.string) value of
                    Err err ->
                        JD.fail "Can't decode event string"

                    Ok event ->
                        case JD.decodeValue (JD.field "payload" JD.string) value of
                            Err _ ->
                                JD.fail "Non-string or missing payload"

                            Ok payload ->
                                let
                                    decoder =
                                        case event of
                                            "update" ->
                                                JD.map UpdateEvent ED.statusDecoder

                                            "notification" ->
                                                JD.map NotificationEvent ED.notificationDecoder

                                            "delete" ->
                                                JD.map DeleteEvent <|
                                                    JD.succeed payload

                                            "filters_changed" ->
                                                JD.succeed FiltersChangedEvent

                                            "connected" ->
                                                JD.succeed <| ConnectedEvent payload

                                            "reconnected" ->
                                                JD.succeed <| ReconnectedEvent payload

                                            "nothing" ->
                                                JD.succeed NoEvent

                                            _ ->
                                                JD.succeed <| UnknownEvent event
                                in
                                case JD.decodeString decoder payload of
                                    Err err ->
                                        JD.fail <| JD.errorToString err

                                    Ok v ->
                                        JD.succeed v
            )
