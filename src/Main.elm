module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
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
    = ShareInPortal
    | SharePublic
    | ShareWithRC
    | ShareSecretLink

type Connection 
    = Reviewing
    | ConnectTo
    | SharedIn
    | External 

type LevelHeader
    = MainHeader
    | SubHeader

type alias Text = 
    List Elm

type alias Paragraph =
    (Span,List Span)

type Elm 
    = Header LevelHeader String
    | Paragraph Paragraph
    | List (List Paragraph)
    | Br

p : Span -> List Span -> Elm
p first rest =
    Paragraph (first,rest)

list : List Paragraph -> Elm
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



render : List Elm -> Html Msg 
render elm = 
    let 
        helper el =
            case el of 
            Header l s ->
                case l of
                    Main -> Html.h1 [] [ Html.text s ]

                    Sub -> Html.h2 [] [ Html.text s ]
            
            Paragraph head tail ->
                let lst = head :: tail in
                Html.div [] 
                (lst |> List.map (\sp -> case sp of
                                    P s -> Html.span [] [ Html.text s ]

                                    I s -> Html.em [ ] [ Html.text s ]

                                    B s -> Html.strong [] [ Html.text s ]))
    in
    Html.div [] (elm |> List.map helper)

   



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
   LevelHeader Main str

br : Elm
br =
    Br


-- MODEL

type Review 
    = Uncategorized
    | Reviewing
    | Revision

type Issue 
    = OpenIssue
    | ClosedIssue
    | ArchivedIssue


type Model
    = NotExist
    | InProgress
    | InReview Review
    | Published PublicationType



init : Model
init =
    0


-- UPDATE

type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]

