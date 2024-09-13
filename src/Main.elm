module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, text)
import Html.Attributes as Attrs exposing (accept)
import Html.Events exposing (onClick)
import List exposing (all)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }


type PublicationType
    = Internal
    | Archive
    | External
    | SelfPublished


type ShareLevel
    = Private
    | ShareInPortal
    | SharePublic
    | ShareInRC


type alias ShareState =
    { level : ShareLevel, secretlink : Bool }


type LevelHeader
    = MainHeader
    | SubHeader


type alias Text =
    List Elm


type Elm
    = Header LevelHeader String
    | Paragraph (List Span)
    | List (List Elm)
    | Br
    | Button Elm Msg
    | Toggle Bool Elm Msg
    | Block (List Elm)
    | Escape (Html Msg)
    | Image { label : Elm, alt : String, url : String }
    | Text (List Span)


image label alt url =
    Image
        { label = label
        , alt = alt
        , url = url
        }


par : List Span -> Elm
par spans =
    Paragraph spans


txt : String -> Elm
txt t =
    Text [ p t ]


list : List Elm -> Elm
list listItems =
    List listItems


hyper : String -> Maybe String -> String -> Span
hyper url text title =
    case text of
        Nothing ->
            L { url = url, text = url, title = title }

        Just t ->
            L { url = url, text = t, title = title }


toggle : Bool -> Elm -> Msg -> Elm
toggle state e msg =
    Toggle state e msg


type Span
    = P String
    | I String
    | B String
    | L { url : String, text : String, title : String }



-- question 1


type ScopeAnswer
    = Journal
    | Exam
    | Assignment
    | Project
    | Informational


render : Elm -> Html Msg
render elm =
    case elm of
        Header l s ->
            case l of
                MainHeader ->
                    Html.h1 [] [ Html.text s ]

                SubHeader ->
                    Html.h2 [] [ Html.text s ]

        Paragraph lst ->
            renderPar lst

        List lst ->
            Html.ul [] <|
                List.map (\e -> Html.li [] [ render e ]) lst

        Br ->
            Html.br [] []

        Button message msg ->
            let
                class =
                    case msg of
                        AuthorMsg _ ->
                            "author"

                        AdminMsg _ ->
                            "admin"

                        _ ->
                            ""
            in
            Html.button [ onClick msg, Attrs.class class ] [ render message ]

        Block lst ->
            Html.div [] <| List.map render lst

        Escape html ->
            html

        Image props ->
            renderImage props

        Text spans ->
            Html.label [] (List.map renderSpan spans)

        Toggle state e msg ->
            renderToggle state e msg


renders : List Elm -> Html Msg
renders elms =
    Html.div [] (elms |> List.map render)


renderPar : List Span -> Html Msg
renderPar pr =
    Html.p []
        (pr
            |> List.map renderSpan
        )


renderSpan : Span -> Html Msg
renderSpan sp =
    case sp of
        P s ->
            Html.span [] [ Html.text s ]

        I s ->
            Html.em [] [ Html.text s ]

        B s ->
            Html.strong [] [ Html.text s ]

        L s ->
            Html.a [ Attrs.href s.url, Attrs.title s.title, Attrs.target "_blank" ] [ Html.text s.text ]


renderImage : { label : Elm, alt : String, url : String } -> Html Msg
renderImage props =
    Html.figure []
        [ Html.img [ Attrs.src props.url, Attrs.alt props.alt ] []
        , Html.figcaption [] [ render props.label ]
        ]


p : String -> Span
p str =
    P str


i : String -> Span
i str =
    I str


b : String -> Span
b str =
    B str


h : String -> Elm
h str =
    Header MainHeader str


br : Elm
br =
    Br


btn : Elm -> Msg -> Elm
btn message msg =
    Button message msg


block : List Elm -> Elm
block lst =
    Block lst



-- MODEL


type Review
    = Uncatagorized
    | BeingReviewed
    | Revision



-- UPDATE


type Msg
    = AdminMsg AdminPublicationAction
    | AuthorMsg AuthorAction
    | Reset


type AuthorAction
    = ChangeShareLevel ShareLevel
    | ChangeSecretLink Bool
    | PublicationAction AuthorPublicationAction
    | ConnectAction ConnectAction
    | CreateExposition
    | DeleteExposition


type AdminPublicationAction
    = AcceptPublication PublicationType
    | RejectPublication
    | PutInRevision
    | PutInReview
    | AssignReviewer
    | Unpublish
    | AcceptConnection
    | RejectConnection


type ConnectAction
    = ConnectToPortal
    | ConnectToGroup


type AuthorPublicationAction
    = SubmitForReview
    | Resubmit
    | SelfPublish


type Issue
    = OpenIssue
    | ClosedIssue
    | ArchivedIssue


type alias Model =
    { current : ExpoStatus
    , history : List ( Msg, ExpoStatus )
    }


type Collaboration
    = NoCollab
    | Collborated


type ExpoStatus
    = NotExist
    | InProgress { share : ShareState, connected : ConnectionStatus, collab : Collaboration }
    | InReview { review : Review, share : ShareState, connected : ConnectionStatus, collab : Collaboration }
    | Published { collab : Collaboration, share : ShareState, publication : PublicationType, connected : ConnectionStatus }


type ConnectionStatus
    = NotConnected
    | ConnectedToPortal Confirmation
    | ConnectedToGroup Confirmation


type Confirmation
    = Unconfirmed
    | Confirmed


init : Model
init =
    { current = NotExist, history = [] }


update : Msg -> Model -> Model
update msg states =
    let
        visibility =
            states.current

        newState : ExpoStatus
        newState =
            case msg of
                Reset ->
                    NotExist

                _ ->
                    case visibility of
                        NotExist ->
                            case msg of
                                AuthorMsg CreateExposition ->
                                    InProgress { share = { level = Private, secretlink = False }, connected = NotConnected, collab = NoCollab }

                                _ ->
                                    visibility

                        InProgress m ->
                            case msg of
                                AuthorMsg (ChangeShareLevel s) ->
                                    let
                                        share =
                                            m.share
                                    in
                                    InProgress { m | share = { share | level = s } }

                                AuthorMsg (ChangeSecretLink bool) ->
                                    let
                                        share =
                                            m.share
                                    in
                                    InProgress { m | share = { share | secretlink = bool } }

                                AuthorMsg (PublicationAction SubmitForReview) ->
                                    InReview { share = m.share, connected = m.connected, collab = m.collab, review = Uncatagorized }

                                AuthorMsg (PublicationAction SelfPublish) ->
                                    Published { share = m.share, connected = m.connected, collab = m.collab, publication = SelfPublished }

                                AuthorMsg (ConnectAction ca) ->
                                    case ca of
                                        ConnectToGroup ->
                                            InProgress { m | connected = ConnectedToGroup Unconfirmed }

                                        ConnectToPortal ->
                                            InProgress { m | connected = ConnectedToPortal Unconfirmed }

                                AdminMsg AcceptConnection ->
                                    InProgress
                                        { m
                                            | connected =
                                                case m.connected of
                                                    ConnectedToGroup _ ->
                                                        ConnectedToGroup Confirmed

                                                    ConnectedToPortal _ ->
                                                        ConnectedToPortal Confirmed

                                                    _ ->
                                                        ConnectedToPortal Confirmed
                                        }

                                AdminMsg RejectConnection ->
                                    InProgress { m | connected = NotConnected }

                                _ ->
                                    visibility

                        InReview m ->
                            case msg of
                                AdminMsg RejectPublication ->
                                    InProgress { share = m.share, connected = m.connected, collab = m.collab }

                                AdminMsg (AcceptPublication ptype) ->
                                    Published { collab = m.collab, connected = m.connected, publication = ptype, share = m.share }

                                AdminMsg PutInRevision ->
                                    InReview { review = Revision, share = m.share, connected = m.connected, collab = m.collab }

                                AdminMsg PutInReview ->
                                    InReview { m | review = BeingReviewed }

                                AdminMsg AssignReviewer ->
                                    InReview { m | review = BeingReviewed }

                                _ ->
                                    visibility

                        Published m ->
                            case msg of
                                AdminMsg Unpublish ->
                                    InProgress { share = m.share, connected = m.connected, collab = m.collab }

                                _ ->
                                    visibility
    in
    case msg of
        Reset ->
            init

        _ ->
            { current = newState, history = ( msg, visibility ) :: states.history }


viewShare : ShareState -> Elm
viewShare sharestate =
    let
        sl =
            sharestate.level

        head =
            p "The exposition has been shared with: "

        tail =
            case sl of
                Private ->
                    p "private to the author and collaborators"

                ShareInPortal ->
                    p "portal members"

                SharePublic ->
                    p "everybody"

                ShareInRC ->
                    p "registered users in RC"
    in
    par [ head, tail ]


renderRadio : List ( Elm, Msg ) -> Html Msg
renderRadio lst =
    Html.select []
        (lst
            |> List.map
                (\( e, msg ) ->
                    Html.option [ onClick msg ] [ render e ]
                )
        )


renderToggle : Bool -> Elm -> Msg -> Html Msg
renderToggle state label msg =
    Html.label [ Html.Events.onClick msg ]
        [ Html.input [ Attrs.type_ "checkbox", Html.Events.onClick msg, Attrs.checked state ] []
        , render label
        ]



-- VIEW


shareLevelToString : ShareLevel -> String
shareLevelToString s =
    case s of
        Private ->
            "private"

        ShareInPortal ->
            "share in portal"

        ShareInRC ->
            "share in rc"

        SharePublic ->
            "public"


dashed =
    String.replace " " "-"


optionsFromShare : ShareLevel -> Elm
optionsFromShare share =
    let
        elmFromShare m =
            Html.text (m |> shareLevelToString)
    in
    Escape
        (Html.div []
            (allShareLevels
                |> List.concatMap
                    (\s ->
                        [ Html.label []
                            [ Html.input [ onClick (AuthorMsg (ChangeShareLevel s)), Attrs.name "share_level", Attrs.type_ "radio", Attrs.value (shareLevelToString s), Attrs.checked (s == share) ] []
                            , elmFromShare s
                            ]
                        , Html.br [] []
                        ]
                    )
            )
        )


allShareLevels : List ShareLevel
allShareLevels =
    [ Private, ShareInPortal, ShareInRC, SharePublic ]


isPublic : ShareLevel -> Bool
isPublic level =
    level == SharePublic


view : Model -> Html Msg
view model =
    let
        status =
            case model.current of
                NotExist ->
                    renders
                        [ h "RC advisor beta version 0.1"
                        , par
                            [ p "A user within your portal can "
                            , hyper "https://guide.researchcatalogue.net/#creating-expositions" (Just "create a new exposition") "help: how to create an exposition"
                            , p " by clicking: "
                            ]
                        , btn (txt "create exposition") (AuthorMsg CreateExposition)
                        ]

                InProgress m ->
                    let
                        sharestate =
                            m.share

                        secretlinkstate =
                            sharestate.secretlink

                        shareBlock =
                            block
                                [ par [ p "The author can now choose to change the visibility by ", hyper "https://guide.researchcatalogue.net/#share" (Just "sharing") "rc documentation", p " it:" ]
                                , optionsFromShare
                                    sharestate.level
                                ]

                        secretLinkCheckbox =
                            if isPublic sharestate.level && secretlinkstate then
                                block
                                    [ toggle secretlinkstate (txt "secret link") (AuthorMsg (ChangeSecretLink (not secretlinkstate)))
                                    , txt " secret link (still works, even while public!)"
                                    ]

                            else
                                toggle secretlinkstate (txt "secret link") (AuthorMsg (ChangeSecretLink (not secretlinkstate)))

                        secretLinkState =
                            if sharestate.secretlink then
                                par [ p "secret link on: the work can be seen by people with the link" ]

                            else
                                par [ p "secret link off" ]

                        request =
                            case m.connected of
                                ConnectedToPortal Unconfirmed ->
                                    List
                                        [ btn (txt "Portal admin may accept the connection") (AdminMsg AcceptConnection)
                                        , btn (txt "Portal admin may reject the connection") (AdminMsg RejectConnection)
                                        ]

                                ConnectedToGroup Unconfirmed ->
                                    List
                                        [ btn (txt "Portal admin may accept the connection") (AdminMsg AcceptConnection)
                                        , btn (txt "Portal admin may reject the connection") (AdminMsg RejectConnection)
                                        ]

                                ConnectedToPortal Confirmed ->
                                    btn (txt "Portal admin may undo the connection") (AdminMsg RejectConnection)

                                ConnectedToGroup Confirmed ->
                                    btn (txt "Portal admin may undo the connection") (AdminMsg RejectConnection)

                                NotConnected ->
                                    txt ""

                        connectBlock =
                            case m.share.level of
                                Private ->
                                    block [ txt "Connection: ", txt "private expositions cannot be connected" ]

                                _ ->
                                    case m.connected of
                                        NotConnected ->
                                            block [ txt "To link the exposition to the portal, the author can: ", br, btn (txt "request connection to portal") (AuthorMsg (ConnectAction ConnectToPortal)) ]

                                        _ ->
                                            txt ""
                    in
                    renders <|
                        [ h "exposition in progress"
                        , br
                        , shareBlock
                        , viewShare m.share
                        , secretLinkCheckbox
                        , secretLinkState
                        , connectBlock
                        , request
                        , br
                        , par [ p "Author may also choose to publish or submit the exposition" ]
                        , list
                            [ block
                                [ btn (txt "self-publish") (AuthorMsg (PublicationAction SelfPublish))
                                , txt "no peer review, creates a fixed reference immediately"
                                ]
                            , block
                                [ btn (txt "Submit to portal") (AuthorMsg (PublicationAction SubmitForReview))
                                , txt "submit the exposition to one of the portals for peer review"
                                ]
                            ]
                        , br
                        , txt "admin may control request"
                        ]

                InReview m ->
                    let
                        reviewActions =
                            case m.review of
                                Uncatagorized ->
                                    block
                                        [ txt "the exposition appears under \"Uncatagorized\", the admin can: "
                                        , br
                                        , btn (txt "add a reviewer") (AdminMsg AssignReviewer)
                                        , btn (txt "put in revision") (AdminMsg PutInRevision)
                                        ]

                                Revision ->
                                    block
                                        [ txt "either the admin or the author resubmits the exposition"
                                        , btn (txt "put in review") (AdminMsg PutInReview)
                                        ]

                                BeingReviewed ->
                                    block
                                        [ txt "the exposition is now in review and can be seen by the reviewer"
                                        , txt "the admin can:"
                                        , br
                                        , btn (txt "put the exposition in revision") (AdminMsg PutInRevision)
                                        , br
                                        , par [ p "when the review is finished, the admin may ", b "accept", p " the publication" ]
                                        , br
                                        , List
                                            [ btn (txt "as an internal publication") (AdminMsg (AcceptPublication Internal))
                                            , btn (txt "as a worldwide publication") (AdminMsg (AcceptPublication External))
                                            , btn (txt "archive it") (AdminMsg (AcceptPublication Archive))
                                            , txt "or"
                                            , btn (txt "reject publication") (AdminMsg RejectPublication)
                                            ]
                                        ]
                    in
                    renders <|
                        [ h <|
                            case m.review of
                                BeingReviewed ->
                                    "In Review"

                                Uncatagorized ->
                                    "In review, but no reviewers assigned"

                                Revision ->
                                    "In revision"
                        , List
                            [ case m.review of
                                Revision ->
                                    txt "The exposition can be edited temporarily"

                                _ ->
                                    txt "The exposition contents are locked"
                            ]
                        , reviewActions
                        , br
                        ]

                Published m ->
                    case m.publication of
                        SelfPublished ->
                            renders
                                [ h "The exposition is self-published"
                                , txt "only SAR can undo this in case of emergencies"
                                ]

                        Archive ->
                            renders <|
                                [ h "the exposition has been archived"
                                , List
                                    [ txt "Only the admin and the author can see it."
                                    , txt "The author has been informed."
                                    , txt "The exposition will no longer be editable, but it may be duplicated through versioning"
                                    ]
                                , List [ btn (txt "unpublish the exposition, put it back \"in progress\"") (AdminMsg Unpublish) ]
                                ]

                        Internal ->
                            renders <|
                                [ h "The exposition has been published internally in the portal."
                                , txt "It can be seen by members of the portal"
                                , txt "It will appear on the portal feed"
                                , List [ txt "The author has been informed. The exposition will no longer be editable, but it may be duplicated through versioning" ]
                                , List [ btn (txt "unpublish the exposition, put it back \"in progress\"") (AdminMsg Unpublish) ]
                                ]

                        External ->
                            renders <|
                                [ h "the exposition is now published it will appear on the front page"
                                , List [ txt "The author has been informed. The exposition will no longer be editable, but it may be duplicated through versioning" ]
                                , List [ btn (txt "unpublish the exposition, put it back \"in progress\"") (AdminMsg Unpublish) ]
                                ]

        rightSide =
            viewHistory model.history

        showSide side =
            Html.div [ Attrs.class "column" ] [ side ]
    in
    Html.div []
        [ render <| btn (txt "reset all") Reset
        , Html.div [ Attrs.class "container" ]
            ([ status, rightSide ] |> List.map showSide)
        ]


viewHistory : List ( Msg, ExpoStatus ) -> Html Msg
viewHistory lst =
    let
        content =
            lst |> List.reverse |> List.indexedMap (\index -> Tuple.first >> viewMsg index) |> List.reverse |> list
    in
    [ h "log of actions: ", content ] |> renders


viewMsg : Int -> Msg -> Elm
viewMsg index msg =
    case msg of
        AuthorMsg action ->
            par [ p (String.fromInt index ++ " "), p "The author ", viewAuthorAction action ]

        AdminMsg action ->
            par [ p (String.fromInt index ++ " "), p "The portal admin ", viewAdminAction action ]

        Reset ->
            par [ p (String.fromInt index ++ " "), p "You have hit reset" ]


viewAuthorAction : AuthorAction -> Span
viewAuthorAction action =
    case action of
        ChangeShareLevel level ->
            viewShareLevel level

        ChangeSecretLink bool ->
            viewSecretLinkState bool

        PublicationAction paction ->
            viewPubAction paction

        ConnectAction connectAction ->
            viewConnectAction connectAction

        CreateExposition ->
            p "created an exposition on their profile"

        DeleteExposition ->
            p "has deleted the exposition."


viewShareLevel : ShareLevel -> Span
viewShareLevel level =
    case level of
        Private ->
            p "changed the share settings to private"

        SharePublic ->
            p "changed the share settings to public"

        ShareInPortal ->
            p "changed the share settings to in portal"

        ShareInRC ->
            p "changed the share settings to rc users"


viewSecretLinkState : Bool -> Span
viewSecretLinkState bool =
    if bool then
        p "has enabled the secret share link"

    else
        p "has disabled the secret share link"


viewPubAction : AuthorPublicationAction -> Span
viewPubAction action =
    case action of
        SubmitForReview ->
            p "submitted the eposition for review by a portal"

        Resubmit ->
            p "resubmitted the exposition"

        SelfPublish ->
            p "self-published the exposition"


viewConnectAction : ConnectAction -> Span
viewConnectAction connect =
    case connect of
        ConnectToGroup ->
            p "requested connection to a group"

        ConnectToPortal ->
            p "requested connection to a portal"


viewAdminAction action =
    case action of
        AcceptPublication level ->
            viewAcceptPublication level

        RejectPublication ->
            p " rejected the request for publication."

        PutInRevision ->
            p " put the exposition in revision"

        PutInReview ->
            p " locked the exposition by putting it back in review"

        AssignReviewer ->
            p " assigned a reviewer to the submitted exposition"

        Unpublish ->
            p " unpublished the exposition"

        AcceptConnection ->
            p " accepted the connection to the portal"

        RejectConnection ->
            p " rejected the connection request"


viewAcceptPublication level =
    let
        rest =
            case level of
                Internal ->
                    "internal publication, only visible to portal users"

                Archive ->
                    "archive, only visible to the portal admin"

                External ->
                    "unlimited, which means fully public"

                SelfPublished ->
                    "self-published through their profile, fully public but not related to the portal"
    in
    p (" accepted publication on the level of " ++ rest)
