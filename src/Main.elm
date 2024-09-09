module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
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
    | Block (List Elm)
    | Escape (Html Msg)
    | Image { label : Elm, alt : String, url : String }


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
    Paragraph [ p t ]


list : List Elm -> Elm
list listItems =
    List listItems


hyper : String -> Maybe String -> String -> Span
hyper url text alt =
    case text of
        Nothing ->
            L { url = url, text = url, alt = alt }

        Just t ->
            L { url = url, text = t, alt = alt }


type Span
    = P String
    | I String
    | B String
    | L { url : String, text : String, alt : String }



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
            Html.button [ onClick msg ] [ render message ]

        Block lst ->
            Html.div [] <| List.map render lst

        Escape html ->
            html

        Image props ->
            renderImage props


renders : List Elm -> Html Msg
renders elms =
    Html.div [] (elms |> List.map render)


renderPar : List Span -> Html Msg
renderPar pr =
    Html.p []
        (pr
            |> List.map
                (\sp ->
                    case sp of
                        P s ->
                            Html.span [] [ Html.text s ]

                        I s ->
                            Html.em [] [ Html.text s ]

                        B s ->
                            Html.strong [] [ Html.text s ]

                        L s ->
                            Html.a [ Attrs.href s.url, Attrs.alt s.alt, Attrs.target "_blank" ] [ Html.text s.text ]
                )
        )


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
    Visibility


type Collaboration
    = NoCollab
    | Collborated


type Visibility
    = NotExist
    | InProgress { share : ShareLevel, connected : ConnectionStatus, collab : Collaboration }
    | InReview { review : Review, share : ShareLevel, connected : ConnectionStatus, collab : Collaboration }
    | Published { collab : Collaboration, share : ShareLevel, publication : PublicationType, connected : ConnectionStatus }


type ConnectionStatus
    = NotConnected
    | ConnectedToPortal Confirmation
    | ConnectedToGroup Confirmation


type Confirmation
    = Unconfirmed
    | Confirmed


init : Model
init =
    NotExist


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            init

        _ ->
            case model of
                NotExist ->
                    case msg of
                        AuthorMsg CreateExposition ->
                            InProgress { share = Private, connected = NotConnected, collab = NoCollab }

                        _ ->
                            model

                InProgress m ->
                    case msg of
                        AuthorMsg (ChangeShareLevel s) ->
                            InProgress { m | share = s }

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

                        _ ->
                            model

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
                            model

                Published m ->
                    case msg of
                        AdminMsg Unpublish ->
                            InProgress { share = m.share, connected = m.connected, collab = m.collab }

                        _ ->
                            model


viewShare : ShareLevel -> Elm
viewShare sl =
    let
        head =
            p "The exposition is visible for: "

        tail =
            case sl of
                Private ->
                    p "authors and collaborators"

                ShareInPortal ->
                    p "users that are members of the portal"

                SharePublic ->
                    p "to all"

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
                            [ Html.input [ onClick (AuthorMsg (ChangeShareLevel s)), Attrs.name "share_level", Attrs.type_ "radio", Attrs.value (shareLevelToString s) ] []
                            , elmFromShare s
                            ]
                        , Html.br [] []
                        ]
                    )
            )
        )


allShareLevels =
    [ Private, ShareInPortal, ShareInRC, SharePublic ]


view : Model -> Html Msg
view model =
    let
        status =
            case model of
                NotExist ->
                    renders [ txt "A user can create a new exposition by clicking: ", btn (txt "create exposition") (AuthorMsg CreateExposition) ]

                InProgress m ->
                    let
                        shareBlock =
                            block
                                [ par [ p "The author can now choose to change the visibility by ", hyper "https://guide.researchcatalogue.net/#share" (Just "sharing") "rc documentation", p " it:" ]
                                , optionsFromShare m.share
                                ]

                        connectBlock =
                            case m.share of
                                Private ->
                                    block [ txt "Connection: ", txt "expositions can be connected to portals or groups, but they have to be shared first." ]

                                ShareInPortal ->
                                    block [ btn (txt "connection to portal") (AuthorMsg (ConnectAction ConnectToPortal)) ]

                                SharePublic ->
                                    block [ btn (txt "connection to portal") (AuthorMsg (ConnectAction ConnectToPortal)) ]

                                ShareInRC ->
                                    block [ btn (txt "connection to portal") (AuthorMsg (ConnectAction ConnectToPortal)) ]
                    in
                    renders <|
                        [ h "There is now an in progress exposition"
                        , txt "it currently exists in the authors profile and has no relationship with the portal."
                        , viewShare m.share
                        , br
                        , shareBlock
                        , br
                        , txt "It is also possible to publish an exposition, which fixes the contents and registers a DOI."
                        , btn (txt "self-publish") (AuthorMsg (PublicationAction SelfPublish))
                        , btn (txt "Submit to portal") (AuthorMsg (PublicationAction SubmitForReview))
                        , br
                        , connectBlock
                        ]

                InReview m ->
                    let
                        reviewActions =
                            case m.review of
                                Uncatagorized ->
                                    block
                                        [ txt "the exposition appears under \"Uncatagorized\""
                                        , btn (txt "add a reviewer") (AdminMsg AssignReviewer)
                                        , btn (txt "put in revision") (AdminMsg PutInRevision)
                                        ]

                                Revision ->
                                    block
                                        [ txt "Either the admin "
                                        , btn (txt "put in review") (AdminMsg PutInReview)
                                        ]

                                BeingReviewed ->
                                    block
                                        [ txt "the exposition is now in review and can be seen by the reviewer"
                                        , btn (txt "add a reviewer") (AdminMsg AssignReviewer)
                                        , btn (txt "put in revision") (AdminMsg PutInRevision)
                                        ]
                    in
                    renders <|
                        [ h "the exposition is now in review."
                        , List
                            [ par [ p "The portal admin has been notified and the exposition is listed in ", b "reviewing" ]
                            , case m.review of
                                Revision ->
                                    txt "The exposition can be edited temporarily"

                                _ ->
                                    txt "The exposition is in review and cannot be edited"
                            ]
                        , reviewActions
                        , br
                        , txt "the admin can now accept this request"
                        , List
                            [ btn (txt "as an internal publication") (AdminMsg (AcceptPublication Internal))
                            , btn (txt "as a worldwide publication") (AdminMsg (AcceptPublication External))
                            , btn (txt "archive it") (AdminMsg (AcceptPublication Archive))
                            , txt "or"
                            , btn (txt "reject publication") (AdminMsg RejectPublication)
                            ]
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
    in
    Html.div [] [ render <| btn (txt "reset all") Reset, status ]
