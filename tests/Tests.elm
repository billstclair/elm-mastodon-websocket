module Tests exposing (all)

import Dict
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List
import Mastodon.EncodeDecode as ED
import Mastodon.Entity as Entity
    exposing
        ( Account
        , Application
        , Attachment
        , AttachmentType(..)
        , Card
        , CardType(..)
        , Context
        , Emoji
        , Entity(..)
        , Field
        , Focus
        , Group
        , GroupRelationship
        , History
        , ImageMetaFields
        , ImageMetaInfo
        , Instance
        , Mention
        , Meta(..)
        , Notification
        , NotificationType(..)
        , Poll
        , PollOption
        , Privacy(..)
        , PushSubscription
        , Relationship
        , ScheduledStatus
        , Source
        , Status
        , StatusParams
        , Tag
        , VideoMetaFields
        , VideoMetaInfo
        , Visibility(..)
        , WrappedAccount(..)
        , WrappedStatus(..)
        )
import Mastodon.WebSocket as WebSocket exposing (Event(..))
import Maybe exposing (withDefault)
import Set exposing (Set)
import Test exposing (..)


testMap : (x -> String -> Test) -> List x -> List Test
testMap test data =
    let
        numbers =
            List.map Debug.toString <| List.range 1 (List.length data)
    in
    List.map2 test data numbers


all : Test
all =
    Test.concat <|
        List.concat
            [ testMap eventTest eventData
            ]


expectResult : Result JD.Error thing -> Result JD.Error thing -> Expectation
expectResult sb was =
    case was of
        Err msg ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True

                Ok _ ->
                    Expect.false (JD.errorToString msg) True

        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True

                Ok sbv ->
                    Expect.equal sbv wasv


stripAccount : Account -> Account
stripAccount account =
    let
        moved =
            case account.moved of
                Nothing ->
                    Nothing

                Just (WrappedAccount wa) ->
                    Just <| WrappedAccount (stripAccount wa)

        source =
            case account.source of
                Nothing ->
                    Nothing

                Just s ->
                    Just { s | v = JE.null }
    in
    { account
        | moved = moved
        , source = source
        , v = JE.null
    }


stripAttachment : Attachment -> Attachment
stripAttachment attachment =
    { attachment | v = JE.null }


stripStatus : Status -> Status
stripStatus status =
    let
        account =
            status.account
    in
    { status
        | account = stripAccount account
        , reblog =
            case status.reblog of
                Nothing ->
                    Nothing

                Just (WrappedStatus ws) ->
                    Just <| WrappedStatus (stripStatus ws)
        , media_attachments =
            List.map stripAttachment status.media_attachments
        , card =
            case status.card of
                Nothing ->
                    Nothing

                Just card ->
                    Just { card | v = JE.null }
        , poll =
            case status.poll of
                Nothing ->
                    Nothing

                Just poll ->
                    Just { poll | v = JE.null }
        , application =
            case status.application of
                Nothing ->
                    Nothing

                Just application ->
                    Just { application | v = JE.null }
        , group =
            case status.group of
                Nothing ->
                    Nothing

                Just group ->
                    Just { group | v = JE.null }
        , quote =
            case status.quote of
                Nothing ->
                    Nothing

                Just (WrappedStatus qs) ->
                    Just (WrappedStatus <| stripStatus qs)
        , v = JE.null
    }


stripContext : Context -> Context
stripContext { ancestors, descendants } =
    { ancestors = List.map stripStatus ancestors
    , descendants = List.map stripStatus descendants
    }


stripNotification : Notification -> Notification
stripNotification notification =
    let
        account =
            notification.account

        status =
            notification.status
    in
    { notification
        | account = stripAccount account
        , status =
            case status of
                Nothing ->
                    Nothing

                Just s ->
                    Just <| stripStatus s
        , v = JE.null
    }


stripEvent : Event -> Event
stripEvent event =
    case event of
        UpdateEvent status ->
            UpdateEvent <| stripStatus status

        NotificationEvent notification ->
            NotificationEvent <| stripNotification notification

        _ ->
            event


eventToString : Event -> String
eventToString event =
    case event of
        NoEvent ->
            ":"

        _ ->
            (\( ev, value ) ->
                JE.object
                    [ ( "event", JE.string ev )
                    , ( "payload", value )
                    ]
                    |> JE.encode 0
            )
            <|
                case event of
                    UpdateEvent status ->
                        ( "update"
                        , ED.encodeStatus status
                            |> JE.encode 0
                            |> JE.string
                        )

                    NotificationEvent notification ->
                        ( "notification"
                        , ED.encodeNotification notification
                            |> JE.encode 0
                            |> JE.string
                        )

                    DeleteEvent id ->
                        ( "delete", JE.string id )

                    FiltersChangedEvent ->
                        ( "filters_changed", JE.string "" )

                    _ ->
                        ( "nothing", JE.string "" )


eventTest : Event -> String -> Test
eventTest event name =
    test ("eventTest \"" ++ name ++ "\"")
        (\_ ->
            let
                string =
                    eventToString event

                result =
                    case WebSocket.decodeEvent string of
                        Err e ->
                            Err e

                        Ok ev ->
                            Ok <| stripEvent ev
            in
            expectResult (Ok event) result
        )


eventData : List Event
eventData =
    [ NoEvent
    , UpdateEvent status1
    , UpdateEvent status2
    , UpdateEvent status3
    , UpdateEvent status4
    , NotificationEvent notification1
    , NotificationEvent notification2
    , NotificationEvent notification3
    , NotificationEvent notification4
    , DeleteEvent "foo"
    , FiltersChangedEvent
    ]


source1 : Source
source1 =
    { privacy = PublicPrivacy
    , sensitive = True
    , language = Just "language"
    , note = "note"
    , fields = [ field1, field2 ]
    , v = JE.null
    }


source2 : Source
source2 =
    { privacy = UnlistedPrivacy
    , sensitive = False
    , language = Nothing
    , note = "note2"
    , fields = []
    , v = JE.null
    }


source3 : Source
source3 =
    { privacy = PrivatePrivacy
    , sensitive = False
    , language = Nothing
    , note = "note3"
    , fields = []
    , v = JE.null
    }


group1 : Group
group1 =
    { id = "id"
    , title = "title"
    , description = "description"
    , cover_image_url = "cover_image_url"
    , is_archived = False
    , member_count = 10
    , v = JE.null
    }


group2 : Group
group2 =
    { id = "id2"
    , title = "title2"
    , description = "description2"
    , cover_image_url = "cover_image_url2"
    , is_archived = True
    , member_count = 20
    , v = JE.null
    }


groupRelationship1 : GroupRelationship
groupRelationship1 =
    { id = "id"
    , member = True
    , admin = False
    , unread_count = 10
    , v = JE.null
    }


groupRelationship2 : GroupRelationship
groupRelationship2 =
    { id = "id2"
    , member = False
    , admin = True
    , unread_count = 20
    , v = JE.null
    }


statusParams1 : StatusParams
statusParams1 =
    { text = "text"
    , in_reply_to_id = Just "in_reply_to_id"
    , media_ids = [ "id1", "id2", "id3" ]
    , sensitive = True
    , spoiler_text = Just "spoiler_text"
    , visibility = Just PublicVisibility
    , scheduled_at = Just "scheduled_at"
    , application_id = "application_id"
    }


statusParams2 : StatusParams
statusParams2 =
    { statusParams1
        | in_reply_to_id = Nothing
        , media_ids = []
        , sensitive = False
        , spoiler_text = Nothing
        , visibility = Just UnlistedVisibility
        , scheduled_at = Nothing
    }


statusParams3 : StatusParams
statusParams3 =
    { statusParams2 | visibility = Just PrivateVisibility }


statusParams4 : StatusParams
statusParams4 =
    { statusParams2 | visibility = Just DirectVisibility }


scheduledStatus1 : ScheduledStatus
scheduledStatus1 =
    { id = "id"
    , scheduled_at = "scheduled_at"
    , params = statusParams1
    , media_attachments = [ attachment1, attachment2 ]
    , v = JE.null
    }


scheduledStatus2 : ScheduledStatus
scheduledStatus2 =
    { scheduledStatus1
        | params = statusParams2
        , media_attachments = []
    }


scheduledStatus3 : ScheduledStatus
scheduledStatus3 =
    { scheduledStatus2 | params = statusParams3 }


scheduledStatus4 : ScheduledStatus
scheduledStatus4 =
    { scheduledStatus2 | params = statusParams4 }


relationship1 : Relationship
relationship1 =
    { id = "id"
    , following = False
    , followed_by = False
    , blocking = False
    , muting = False
    , muting_notifications = False
    , requested = False
    , domain_blocking = False
    , showing_reblogs = False
    , endorsed = False
    , v = JE.null
    }


relationship2 : Relationship
relationship2 =
    { id = "id"
    , following = True
    , followed_by = True
    , blocking = True
    , muting = True
    , muting_notifications = True
    , requested = True
    , domain_blocking = True
    , showing_reblogs = True
    , endorsed = True
    , v = JE.null
    }


pushSubscription1 : PushSubscription
pushSubscription1 =
    { id = "id"
    , endpoint = "endpoint"
    , server_key = "server_key"
    , alerts = JE.string "alerts"
    , v = JE.null
    }


notification1 : Notification
notification1 =
    { id = "id"
    , type_ = FollowNotification
    , created_at = "created_at"
    , account = account1
    , status = Just status1
    , v = JE.null
    }


notification2 : Notification
notification2 =
    { notification1
        | type_ = MentionNotification
        , status = Nothing
    }


notification3 : Notification
notification3 =
    { notification2
        | type_ = ReblogNotification
    }


notification4 : Notification
notification4 =
    { notification2
        | type_ = FavouriteNotification
    }


card1 : Card
card1 =
    { url = "url"
    , title = "title"
    , description = "description"
    , image = Nothing
    , type_ = LinkCard
    , author_name = Nothing
    , author_url = Nothing
    , provider_name = Nothing
    , provider_url = Nothing
    , html = Just "html"
    , width = Nothing
    , height = Nothing
    , v = JE.null
    }


card2 : Card
card2 =
    { url = "url2"
    , title = "title2"
    , description = "description2"
    , image = Just "image"
    , type_ = PhotoCard
    , author_name = Nothing
    , author_url = Nothing
    , provider_name = Nothing
    , provider_url = Nothing
    , html = Nothing
    , width = Just 1024
    , height = Just 768
    , v = JE.null
    }


card3 : Card
card3 =
    { url = "url3"
    , title = "title3"
    , description = "description3"
    , image = Just "image"
    , type_ = VideoCard
    , author_name = Nothing
    , author_url = Nothing
    , provider_name = Nothing
    , provider_url = Nothing
    , html = Nothing
    , width = Just 1536
    , height = Just 1024
    , v = JE.null
    }


card4 : Card
card4 =
    { url = "url"
    , title = "title"
    , description = "description"
    , image = Just "image"
    , type_ = RichCard
    , author_name = Just "author_name"
    , author_url = Just "author_url"
    , provider_name = Just "provider_name"
    , provider_url = Just "provider_url"
    , html = Nothing
    , width = Nothing
    , height = Nothing
    , v = JE.null
    }


application1 : Application
application1 =
    { name = "name"
    , website = Nothing
    , v = JE.null
    }


application2 : Application
application2 =
    { name = "name2"
    , website = Just "website"
    , v = JE.null
    }


status1 : Status
status1 =
    { id = "id"
    , uri = "uri"
    , url = Just "url"
    , account = account1
    , in_reply_to_id = Just "in_reply_to_id"
    , in_reply_to_account_id = Just "in_reply_to_account_id"
    , reblog = Just <| WrappedStatus status2
    , content = "content"
    , plain_markdown = Nothing
    , plain_text = Nothing
    , created_at = "created_at"
    , emojis = [ emoji1, emoji2 ]
    , replies_count = 100
    , reblogs_count = 10
    , favourites_count = 1000
    , reblogged = True
    , favourited = True
    , muted = False
    , sensitive = False
    , spoiler_text = "spoiler_text"
    , visibility = PublicVisibility
    , media_attachments = [ attachment1, attachment2 ]
    , mentions = [ mention1, mention2 ]
    , tags = [ tag1, tag2 ]
    , card = Just card1
    , poll = Just poll1
    , application = Just application1
    , language = Just "language"
    , pinned = True
    , group = Nothing
    , quote_of_id = Nothing
    , quote = Nothing
    , v = JE.null
    }


status2 : Status
status2 =
    { id = "id2"
    , uri = "uri2"
    , url = Nothing
    , account = account2
    , in_reply_to_id = Nothing
    , in_reply_to_account_id = Nothing
    , reblog = Nothing
    , content = "content2"
    , plain_markdown = Nothing
    , plain_text = Nothing
    , created_at = "created_at2"
    , emojis = []
    , replies_count = 0
    , reblogs_count = 0
    , favourites_count = 0
    , reblogged = False
    , favourited = False
    , muted = True
    , sensitive = True
    , spoiler_text = "spoiler_text2"
    , visibility = UnlistedVisibility
    , media_attachments = []
    , mentions = []
    , tags = []
    , card = Just card2
    , poll = Just poll2
    , application = Just application2
    , language = Nothing
    , pinned = False
    , group = Just group1
    , quote_of_id = Nothing
    , quote = Nothing
    , v = JE.null
    }


status3 : Status
status3 =
    { status2
        | id = "id3"
        , visibility = PrivateVisibility
        , card = Nothing
        , application = Nothing
        , quote_of_id = Just "id2"
        , quote = Just <| WrappedStatus status2
    }


status4 : Status
status4 =
    { status2
        | visibility = DirectVisibility
        , card = Nothing
    }


poll1 : Poll
poll1 =
    { id = "id"
    , expires_at = Just "expires_at"
    , expired = False
    , multiple = False
    , votes_count = 10
    , options = [ pollOption1, pollOption2 ]
    , voted = False
    , v = JE.null
    }


poll2 : Poll
poll2 =
    { id = "id2"
    , expires_at = Nothing
    , expired = True
    , multiple = True
    , votes_count = 100
    , options = [ pollOption1, pollOption2 ]
    , voted = True
    , v = JE.null
    }


pollOption1 : PollOption
pollOption1 =
    { title = "Yes"
    , votes_count = 10
    }


pollOption2 : PollOption
pollOption2 =
    { title = "No"
    , votes_count = 1000
    }


tag1 : Tag
tag1 =
    { name = "name"
    , url = "url"
    , history = [ history1, history2 ]
    }


tag2 : Tag
tag2 =
    { name = "name2"
    , url = "url2"
    , history = []
    }


history1 : History
history1 =
    { day = "day"
    , uses = 1
    , accounts = 2
    }


history2 : History
history2 =
    { day = "day2"
    , uses = 3
    , accounts = 4
    }


mention1 : Mention
mention1 =
    { url = "url"
    , username = "username"
    , acct = "acct"
    , id = "id"
    }


mention2 : Mention
mention2 =
    { url = "url2"
    , username = "username2"
    , acct = "acct2"
    , id = "id2"
    }


attachment1 : Attachment
attachment1 =
    { id = "id1"
    , type_ = UnknownAttachment
    , url = "url"
    , remote_url = Just "remote_url"
    , preview_url = Just "preview_url"
    , text_url = Just "text_url"
    , meta = Nothing
    , description = Nothing
    , v = JE.null
    }


attachment2 : Attachment
attachment2 =
    { id = "id2"
    , type_ = ImageAttachment
    , url = "url2"
    , remote_url = Nothing
    , preview_url = Just "preview_url2"
    , text_url = Nothing
    , meta = Just imageMeta1
    , description = Just "description2"
    , v = JE.null
    }


attachment3 : Attachment
attachment3 =
    { id = "id3"
    , type_ = ImageAttachment
    , url = "url3"
    , remote_url = Nothing
    , preview_url = Just "preview_url3"
    , text_url = Just "text_url3"
    , meta = Just imageMeta2
    , description = Just "description3"
    , v = JE.null
    }


attachment4 : Attachment
attachment4 =
    { id = "id4"
    , type_ = VideoAttachment
    , url = "url4"
    , remote_url = Nothing
    , preview_url = Just "preview_url4"
    , text_url = Nothing
    , meta = Just videoMeta1
    , description = Nothing
    , v = JE.null
    }


attachment5 : Attachment
attachment5 =
    { id = "id5"
    , type_ = VideoAttachment
    , url = "url5"
    , remote_url = Nothing
    , preview_url = Just "preview_url5"
    , text_url = Nothing
    , meta = Just videoMeta2
    , description = Just "description5"
    , v = JE.null
    }


imageMeta1 : Meta
imageMeta1 =
    ImageMeta
        { small = Just imageMetaInfo1
        , original = Nothing
        , focus = Just focus
        }


imageMeta2 : Meta
imageMeta2 =
    ImageMeta
        { small = Nothing
        , original = Just imageMetaInfo2
        , focus = Nothing
        }


imageMetaInfo1 : ImageMetaInfo
imageMetaInfo1 =
    { width = Just 1
    , height = Just 2
    , size = Nothing
    , aspect = Nothing
    }


imageMetaInfo2 : ImageMetaInfo
imageMetaInfo2 =
    { width = Nothing
    , height = Nothing
    , size = Just "128"
    , aspect = Just 0.5
    }


videoMeta1 : Meta
videoMeta1 =
    VideoMeta
        { small = Just videoMetaInfo1
        , original = Nothing
        , focus = Just focus
        }


videoMeta2 : Meta
videoMeta2 =
    VideoMeta
        { small = Nothing
        , original = Just videoMetaInfo2
        , focus = Nothing
        }


videoMetaInfo1 : VideoMetaInfo
videoMetaInfo1 =
    { width = Just 3
    , height = Just 4
    , frame_rate = Nothing
    , duration = Nothing
    , bitrate = Nothing
    }


videoMetaInfo2 : VideoMetaInfo
videoMetaInfo2 =
    { width = Nothing
    , height = Nothing
    , frame_rate = Just "30"
    , duration = Just 12.3
    , bitrate = Just 10000
    }


focus : Focus
focus =
    { x = 0.5
    , y = 0.5
    }


account1 : Account
account1 =
    { id = "id"
    , username = "username"
    , acct = "acct"
    , display_name = "display_name"
    , locked = True
    , created_at = "created_at"
    , followers_count = 1000
    , following_count = 100
    , statuses_count = 500
    , note = "note"
    , url = "url"
    , avatar = "avatar"
    , avatar_static = "avatar_static"
    , header = "header"
    , header_static = "header_static"
    , emojis = [ emoji1, emoji2 ]
    , moved = Nothing
    , fields =
        [ field1
        , field2
        ]
    , bot = True
    , source = Just source1
    , is_pro = True
    , is_verified = True
    , is_donor = True
    , is_investor = True
    , v = JE.null
    }


emoji1 : Emoji
emoji1 =
    { shortcode = "shortcode"
    , static_url = "static_url"
    , url = "url2"
    , visible_in_picker = False
    }


emoji2 : Emoji
emoji2 =
    { shortcode = "shortcode 2"
    , static_url = "static_url 2"
    , url = "url3"
    , visible_in_picker = True
    }


field1 : Field
field1 =
    { name = "name"
    , value = "value"
    , verified_at = Just "verified_at"
    }


field2 : Field
field2 =
    { name = "name2"
    , value = "value2"
    , verified_at = Nothing
    }


account2 : Account
account2 =
    { id = "id2"
    , username = "username2"
    , acct = "acct2"
    , display_name = "display_name2"
    , locked = True
    , created_at = "created_at2"
    , followers_count = 1001
    , following_count = 101
    , statuses_count = 501
    , note = "note2"
    , url = "url2"
    , avatar = "avatar2"
    , avatar_static = "avatar_static2"
    , header = "header2"
    , header_static = "header_static2"
    , emojis = []
    , moved = Just (WrappedAccount account1)
    , fields = []
    , bot = True
    , source = Nothing
    , is_pro = False
    , is_verified = False
    , is_donor = False
    , is_investor = False
    , v = JE.null
    }
