module Pages.CTF.View exposing (acc, cardView, displayPlaceholder, flagChildren, flagTitle, getElem, rowMap, sidebarContent, updateFlagAlert, view, viewFlag, viewScore)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.ListGroup as ListGroup
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.CTF.Model exposing (..)
import Pages.CTF.Update exposing (..)
import Ui.Icon exposing (..)
import Utils exposing (..)



--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------


updateFlagAlert : Flag -> Html Msg
updateFlagAlert obj =
    Alert.config
        |> Alert.info
        |> Alert.children
            [ Alert.h4 [] [ text "You are correct!" ]
            ]
        |> Alert.view obj.alert


displayPlaceholder f response =
    if f.captured then
        f.answer

    else
        "Type an answer"


cardView m response i obj =
    [ Card.config
        [ if obj.captured then
            Card.success

          else if obj.color == "white" then
            Card.light

          else
            Card.danger
        ]
        |> Card.header [] [ h2 [ onClick (ExpandFlag m.curr_section i) ] [ text <| obj.title ++ " (" ++ String.fromInt obj.value ++ "pts)" ] ]
        |> Card.block []
            [ Block.titleH4 [] [ text obj.description ]
            , Block.custom <| input [ placeholder (displayPlaceholder obj response), onInput (UpdateResponse i), value response, readonly obj.captured ] []
            , Block.custom <| Button.button [ Button.secondary, Button.onClick (SendResponse i m.curr_section response), Button.disabled obj.captured ] [ text "Send" ]
            , Block.custom <| updateFlagAlert obj
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


rowMap =
    htmlIndexedMap div p


sidebarContent : Model -> List (Html Msg)
sidebarContent m =
    List.map (\y -> p [] [ a [ href "#", onClick (GetOneSectionAPI <| Maybe.withDefault 99999999 <| String.toInt y) ] [ text ("Section " ++ y) ] ]) <| List.map String.fromInt <| List.map .idSection m.sections


view : Model -> Browser.Document Msg
view m =
    { title = "Capture The Flag"
    , body =
        [ div []
            [ CDN.stylesheet
            , div [ class "topnav" ] [ img [ src "image.jpeg", width 175, height 60 ] [] ]
            , div [ class "container" ]
                [ div [ class "sidenav" ] (sidebarContent m)
                , div [ class "score" ]
                    [ p []
                        [ text (viewScore m)
                        ]
                    ]
                , div [ class "flagcontainer" ]
                    [ rowMap (viewFlag m m.response m.curr_section.expanded) m.curr_section.flags
                    ]
                , div [ class "row" ]
                    [ Html.form
                        [ onSubmit SendPlayer
                        , class "form-container"
                        ]
                        [ label []
                            [ text "Apelido"
                            , input
                                [ type_ "text"
                                , placeholder "Apelido"
                                , onInput UpdatePlayerAlias
                                , value m.player.alias
                                ]
                                []
                            ]
                        , label []
                            [ text "Score"
                            , input
                                [ value <| String.fromInt m.player.score
                                , hidden True
                                ]
                                []
                            ]
                        , button
                            []
                            [ text "Submit" ]
                        ]
                    ]
                ]
            ]
        ]
    }


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
    div [ style "background-color" obj.color ] (flagTitle m i obj :: body)


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
    Maybe.withDefault 0 (getElem (m.curr_section.expanded + 1) (List.map .value m.curr_section.flags))


flagTitle m i obj =
    h2 [ onClick (ExpandFlag m.curr_section i) ]
        [ text obj.title
        ]


flagChildren m response i obj =
    [ p [] [ text obj.description ]
    , div []
        [ text "Answer: "
        , input [ placeholder "42", onInput (UpdateResponse i), value response, readonly obj.captured ] []
        , Button.button [ Button.secondary, Button.onClick (SendResponse i m.curr_section response), Button.disabled obj.captured ] [ text "Send" ]
        ]
    ]
