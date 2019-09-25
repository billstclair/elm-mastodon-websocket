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
    , streamUrl, decodeEvent
    )

{-| The WebSocket API for the Mastodon Social Network.


# Types

@docs StreamType, Event


# Functions

@docs streamUrl, decodeEvent

-}

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Mastodon.EncodeDecode as ED
import Mastodon.Entity exposing (Notification, Status)
import Mastodon.PortFunnels exposing (FunnelDict, Handler(..), State)
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


{-| Convert a host and a stream type into a URL for the WebSocket connection.
-}
streamUrl : String -> StreamType -> String
streamUrl host streamType =
    Builder.crossOrigin
        ("https://" ++ host)
        [ "api", "v1", "streaming" ]
        (streamTypeToQueryParameters streamType)


streamTypeToQueryParameters : StreamType -> List QueryParameter
streamTypeToQueryParameters streamType =
    case streamType of
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


{-| An event received over the websocket.
-}
type Event
    = NoEvent
    | UpdateEvent Status
    | NotificationEvent Notification
    | DeleteEvent String
    | FiltersChangedEvent


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
