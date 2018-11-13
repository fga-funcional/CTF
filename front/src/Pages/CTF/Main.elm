module Pages.CTF.Main exposing (..)

import Browser
import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Icon exposing (..)
import Utils exposing (..)
import Json.Decode as D
import Json.Encode as E
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



--------------------------------------------------------------------------------
-- MODEL
--------------------------------------------------------------------------------


type alias Model =
    { flags : List Flag
    , expanded : Int
    , response : String
    , player : Player
    }


type alias Flag =
    { color : String
    , value : Int
    , title : String
    , captured : Bool
    , answer : String
    , description : String
    }


type alias Player =
    { score : Int
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] 0 "" { score = 0 },  Http.send GotFlagsAPI getFlags)

{-| Create simple flag element
-}
flag : String -> Int -> String -> Bool -> String -> String -> Flag
flag color value title captured answer descr =
    Flag color value title False answer descr



--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | Expand Int
    | UpdateResponse Int String
    | SendResponse Int Flag String
    | GetFlagsAPI 
    | GotFlagsAPI (Result Http.Error (List Flag))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            (m, Cmd.none)
        GetFlagsAPI ->
            ( m, Http.send GotFlagsAPI getFlags )
        GotFlagsAPI result ->
            case result of
                Err httpError ->
                    let
                        _ =
                            "FlagError"
                    in
                        ( m, Cmd.none )

                Ok flags ->
                    ( { m | flags = flags }, Cmd.none )
        Expand i ->
            ( { m | expanded = i }, Cmd.none )

        UpdateResponse i st ->
            ( { m | response = st }, Cmd.none )

        SendResponse i f ans ->
            let
                p =
                    m.player
                k = m.flags
                value = 
                    if ans == f.answer then
                        f.value
                    else
                        0
            in
            ( { m | response = "", player = { p | score = p.score + value}, flags = (updateFlagList k ans m.expanded) }, Cmd.none )

updateFlagList : List Flag -> String -> Int -> List Flag
updateFlagList lista ans indexTo =
    let
        toggle index fla =
            if index == indexTo then
                let
                    answ = fla.answer 
                   
                    in   
                        if ans == answ then
                            {fla | color = "green", captured = True}
                        else    
                            {fla | color = "red", captured = False}
            else
                { fla | color = fla.color}
    in
        List.indexedMap toggle lista

updateFlag : String -> Flag -> Flag
updateFlag ans f =   
     let
      answ = f.answer   
     in   
        if ans == answ then
            {f | color = "green"}
        else    
            {f | color = "red"}


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------

displayPlaceholder f response= 
    if f.captured then
        f.answer
    else
        "Type an answer"

cardView m response i obj =
    [Card.config [if obj.captured then Card.success else if obj.color == "white" then Card.light else Card.danger] 
        |> Card.header [] [h2 [onClick (Expand i)] [text <| obj.title ++ " (" ++ String.fromInt obj.value ++ "pts)"]]
        |> Card.block []
            [
              Block.titleH4 [] [ text obj.description]
            , Block.custom <| input [ placeholder (displayPlaceholder obj response), onInput (UpdateResponse i), value response, readonly obj.captured ] []
            , Block.custom <| Button.button [ Button.secondary, Button.onClick (SendResponse i obj response), Button.disabled obj.captured ] [ text "Send" ]
            ]
        |> Card.view
    ]

-- flagView : Flag -> Html Msg
-- flagView currFlag = 
--     ListGroup.ul [ListGroup.li []
--         [ span []
--             [ b [] [ text "Title: "] 
--             , p [] [text currFlag.title]
--             ]
--         ]
--     ]

view : Model -> Html Msg
view m =
    div []
        [ CDN.stylesheet
        , div [ class "jumbotron"]
            [ h1 [] [ text "CTF Competition" ]
            , ulIndexedMap (viewFlag m m.response m.expanded) m.flags
            ]
        , div []
            [ h1 [] [ text (viewScore m) ]
            ]
        , Button.button [ Button.primary, Button.onClick (GetFlagsAPI) ] [ text "Get Flags" ]
        ]


viewScore : Model -> String
viewScore m =
    "Score: " ++ String.fromInt m.player.score


viewFlag : Model -> String -> Int -> Int -> Flag -> Html Msg
viewFlag m response expanded i obj =
    let
        body =
            if i == expanded then
                cardView m response i obj

            else
                []
    in
    div [style "background-color" obj.color] (flagTitle i obj :: body)


getElem : Int -> List a -> Maybe a
getElem index list =
    -- 3 [ 1, 2, 3, 4, 5, 6 ]
    if List.length list >= index then
        List.take index list
            -- [ 1, 2, 3 ]
            |> List.reverse
            -- [ 3, 2, 1 ]
            |> List.head
        -- Just 3

    else
        Nothing



--Stackoverflow


acc : Model -> Int
acc m =
    Maybe.withDefault 0 (getElem m.expanded (List.map .value m.flags))


flagTitle i obj =
    h2 [ onClick (Expand i) ]
        [ text obj.title
        ]


flagChildren response i obj =
    [ p [] [ text obj.description ]
    , div []
        [ text "Answer: "
        , input [ placeholder "42", onInput (UpdateResponse i), value response, readonly obj.captured ] []
        , Button.button [ Button.secondary, Button.onClick (SendResponse i obj response), Button.disabled obj.captured ] [ text "Send" ]
        ]
    ]


----------------------------
-- Decoders
----------------------------

flagDecoder : D.Decoder (List Flag)
flagDecoder =
    D.list (D.map6 flag
        (D.at ["color"] D.string)
        (D.at ["value"] D.int)
        (D.at ["title"] D.string)
        (D.at ["captured"] D.bool)
        (D.at ["answer"] D.string)
        (D.at ["description"] D.string)
    )


getFlags : Http.Request (List Flag)
getFlags =
  Http.get "http://localhost:3000/flags" (flagDecoder)

--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------


-- example : Model
-- example =
--     { init
--         | flags =
--             [ flag "Fibonacci" "Find some fibonacci numbers" 1 "white" False
--             , flag "Collatz" "Sequence of numbers" 2 "white" False
--             , flag "Factorial" "Blah" 2 "white" False
--             ]
--     }

