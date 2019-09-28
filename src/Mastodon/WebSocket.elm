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
    , streamUrl, open, close, decodeEvent
    )

{-| The WebSocket API for the Mastodon Social Network.


# Types

@docs StreamType, Event


# Functions

@docs streamUrl, open, close, decodeEvent

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
-}
type Event
    = NoEvent
    | UpdateEvent Status
    | NotificationEvent Notification
    | DeleteEvent String
    | FiltersChangedEvent
      -- A message that can't be decoded
    | UnknownEvent String
      -- When the socket connection is closed
    | ClosedEvent String


{-| Decode a string from the WebSocket stream.
-}
decodeEvent : String -> Result JD.Error Event
decodeEvent string =
    if ":" == String.left 1 string then
        Ok NoEvent

    else
        JD.decodeString eventDecoder string


eventDecoder : Decoder Event
eventDecoder =
    JD.field "event" JD.string
        |> JD.andThen
            (\event ->
                case event of
                    "update" ->
                        JD.map UpdateEvent
                            (JD.field "data" ED.statusDecoder)

                    "notification" ->
                        JD.map NotificationEvent
                            (JD.field "data" ED.notificationDecoder)

                    "delete" ->
                        JD.map DeleteEvent
                            (JD.field "data" JD.string)

                    "filters_changed" ->
                        JD.succeed FiltersChangedEvent

                    _ ->
                        JD.fail <| "Unknown event: " ++ event
            )
