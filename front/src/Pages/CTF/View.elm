module Pages.CTF.View exposing (..)
import Pages.CTF.Model exposing (..)
import Pages.CTF.Update exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Icon exposing (..)
import Utils exposing (..)
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN


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
