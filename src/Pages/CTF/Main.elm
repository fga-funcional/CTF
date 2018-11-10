module Pages.CTF.Main exposing (Model, getElem, init, main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Icon exposing (..)
import Utils exposing (..)


main =
    Browser.sandbox
        { init = example
        , view = view
        , update = update
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
    { title : String
    , description : String
    , value : Int
    , color : String
    , captured : Bool
    }


type alias Player =
    { score : Int
    }


init : Model
init =
    { flags = [], expanded = 0, response = "", player = { score = 0 } }


{-| Create simple flag element
-}
flag : String -> String -> Int -> String -> Flag
flag title descr value color =
    Flag title descr value color False



--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | Expand Int
    | UpdateResponse Int String
    | SendResponse Int Flag


update : Msg -> Model -> Model
update msg m =
    case msg of
        Expand i ->
            { m | expanded = i }

        UpdateResponse i st ->
            { m | response = st }

        SendResponse i f ->
            let
                p =
                    m.player
                k = m.flags
            in
            { m | response = "", player = { p | score = p.score + f.value }, flags = (updateFlagList k m.expanded) }
        _ ->
            m

updateFlagList : List Flag -> Int -> List Flag
updateFlagList lista indexTo =
    let
        toggle index fla =
            if index == indexTo then
                {fla | color = "red", captured = True}
            else
                { fla | color = fla.color}
    in
        List.indexedMap toggle lista

updateFlag : Flag -> Flag
updateFlag f = 
    {f | color = "red"}

--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ div []
            [ h1 [] [ text "CTF Competition" ]
            , ulIndexedMap (viewFlag m.response m.expanded) m.flags
            ]
        , div []
            [ h1 [] [ text (viewScore m) ]
            ]
        ]


viewScore : Model -> String
viewScore m =
    "Score: " ++ String.fromInt m.player.score


viewFlag : String -> Int -> Int -> Flag -> Html Msg
viewFlag response expanded i obj =
    let
        body =
            if i == expanded then
                flagChildren response i obj

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
        , text <| " (" ++ String.fromInt obj.value ++ "pts)"
        ]


flagChildren response i obj =
    [ p [] [ text obj.description ]
    , div []
        [ text "Answer: "
        , input [ placeholder "42", onInput (UpdateResponse i), value response, readonly obj.captured ] []
        , button [ onClick (SendResponse i obj), disabled obj.captured ] [ text "Send" ]
        ]
    ]



--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------


example : Model
example =
    { init
        | flags =
            [ flag "Fibonacci" "Find some fibonacci numbers" 1 "white"
            , flag "Collatz" "Sequence of numbers" 2 "white"
            , flag "Factorial" "Blah" 2 "white"
            ]
    }
