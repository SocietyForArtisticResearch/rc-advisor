module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (accept)
import Html.Events exposing (onClick)



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
    | ShareWithRC


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


par : List Span -> Elm
par spans =
    Paragraph spans


txt : String -> Elm
txt t =
    Paragraph [ p t ]


list : List Elm -> Elm
list listItems =
    List listItems


type Span
    = P String
    | I String
    | B String



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
            Html.div []
                (lst
                    |> List.map
                        (\sp ->
                            case sp of
                                P s ->
                                    Html.span [] [ Html.text s ]

                                I s ->
                                    Html.em [] [ Html.text s ]

                                B s ->
                                    Html.strong [] [ Html.text s ]
                        )
                )

        List lst ->
            Html.ul [] <|
                List.map (\e -> Html.li [] [ render e ]) lst

        Br ->
            Html.br [] []

        Button message msg ->
            Html.button [ onClick msg ] [ render message ]


renders : List Elm -> Html Msg
renders elms =
    Html.div [] (elms |> List.map render)


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
            p "Share level is now: "

        tail =
            case sl of
                Private ->
                    p "private: only visible to the authors and collaborators"

                ShareInPortal ->
                    p "shared in portal: only visible to members of the portal"

                SharePublic ->
                    p "public: visible to all"

                ShareWithRC ->
                    p "shared with rc"
    in
    par [ head, tail ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        status =
            case model of
                NotExist ->
                    renders [ h "does not exist insert option to create exposition here ", btn (txt "create exposition") (AuthorMsg CreateExposition) ]

                InProgress m ->
                    renders <|
                        [ h "There is now an in progress exposition"
                        , viewShare m.share
                        , br
                        , btn (txt "Share in portal") (AuthorMsg (ChangeShareLevel ShareInPortal))
                        , btn (txt "Submit to portal") (AuthorMsg (PublicationAction SubmitForReview))
                        ]

                InReview m ->
                    renders <|
                        [ h "the exposition is now in review."
                        , List [ txt "The portal admin has been notified.", txt "The exposition content is locked" ]
                        , br
                        , h "the admin can now"
                        , List [ btn (txt "accept the publication as an internal publication") (AdminMsg (AcceptPublication Internal)) ]
                        , List [ btn (txt "accept the publication as a fully public") (AdminMsg (AcceptPublication External)) ]
                        , List [ btn (txt "accept the publication, but only archive it") (AdminMsg (AcceptPublication Archive)) ]
                        ]

                Published m ->
                    renders <|
                        [ h "the exposition is now published"
                        , List [ txt "The author has been informed. The exposition will no longer be editable, but it may be duplicated through versioning" ]
                        , List [ btn (txt "unpublish the exposition, put it back \"in progress\"") (AdminMsg Unpublish) ]
                        ]
    in
    Html.div [] [ render <| btn (txt "reset all") Reset, status ]
