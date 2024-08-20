module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


-- MAIN

main =
    Browser.sandbox { init = init, update = update, view = view }


type Publication 
    = Internal 
    | Archive
    | External

type Share 
    = ShareInPortal
    | SharePublic
    | ShareWithRC
    | ShareSecretLink

type Functionality 
    = Publication Publication
    | Reviewing
    | Sharing Share
    | Collaborate
    | Group 
    | SelfPublication

type Level 
    = Main
    | Sub

type Elm 
    = Header Level String
    | Paragraph Span (List Span)

type Span 
    = P String
    | I String
    | B String


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

-- MODEL

type alias Model =
    Int


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

