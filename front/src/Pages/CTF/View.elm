module Pages.CTF.View exposing (acc, cardView, displayPlaceholder, flagChildren, flagTitle, getElem, rowMap, sidebarContent, updateFlagAlert, view, viewFlag, viewScore)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Utilities.Display as Display
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

displayPlaceholder : Flag -> String -> String
displayPlaceholder f response =
    if f.captured then
        f.answer

    else
        "Type an answer"

cardView : Model -> String -> Int -> Flag -> List (Html Msg)
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

rowMap : (Int -> a -> Html msg) -> List a -> Html msg
rowMap =
    htmlIndexedMap div p

sidebarContent : Model -> List (Html Msg)
sidebarContent m =
    List.map (\y -> p [] [ a [ href "#", onClick (GetOneSectionAPI <| Maybe.withDefault 99999999 <| String.toInt y) ] [ text (sidebarContentTexts m (Maybe.withDefault 999999 <| String.toInt y)) ] ]) <| List.map String.fromInt <| List.map .idSection m.sections

sidebarContentTexts : Model -> Int -> String
sidebarContentTexts m id =  Maybe.withDefault "" <| getElem id  <| List.map .name m.sections

-- flagRows : Model -> List (Html Msg)
-- flagRows m =
--     List.map (\x -> div[] [rowMap (viewFlag m m.response m.curr_section.expanded) m.curr_section.flags])



view : Model -> Browser.Document Msg
view m =
    { title = "Capture The Flag"
    , body =
        [ div []
            [ CDN.stylesheet
            , div [ class "topnav" ] [ img [ src "image.jpeg", width 175, height 60 ] [] ]
            , div [ class "container" ]
                [ div [ class "sidenav" ] (List.append (sidebarContent m) <| [button[onClick ChangeRank][text "Mostrar rank"]])
                , div [ class "score" ]
                    [ formPlayer m
                    , p []
                        [ text (viewScore m)
                        ]
                    ]
                , div[class "rankcontainer", style "display" m.showRank] [
                    tableRanking m
                ], div [ class "flagcontainer", style "display" (m.showFlags)]
                    [rowMap (viewFlag m m.response m.curr_section.expanded) m.curr_section.flags]
                ]
            ]
        ]
    }


tableRanking : Model -> Html msg
tableRanking m = 
    table [ class "table table-striped", align "center"]
            (List.map (\x -> tr [] [td [] [text x.alias],
                td[] [text <| String.fromInt x.score] ]) m.ranking)

formPlayer : Model -> Html Msg         
formPlayer m = div [ class "row" ]
                    [ Html.form
                        [ onSubmit SendPlayer
                        , class "form-container"
                        ]
                        [ label []
                            [ text "Apelido "
                            , input
                                [ type_ "text"
                                , placeholder "Apelido"
                                , onInput UpdatePlayerAlias
                                , value m.player.alias
                                ]
                                []
                            ]
                        , label []
                            [ 
                                input
                                [ value <| String.fromInt m.player.score
                                , hidden True
                                ]
                                []
                            ]
                        , button
                            [disabled <| validadeStringfield m.player.alias ]
                            [ text "Submit" ]
                        ]
                    ]

validadeStringfield : String -> Bool
validadeStringfield s =
    if String.trim s == "" then
        True
    else
        False

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

flagTitle : Model -> Int -> Flag -> Html Msg
flagTitle m i obj =
    h2 [ onClick (ExpandFlag m.curr_section i) ]
        [ text obj.title
        ]

flagChildren : Model -> String -> Int -> Flag -> List (Html Msg)
flagChildren m response i obj =
    [ p [] [ text obj.description ]
    , div []
        [ text "Answer: "
        , input [ placeholder "42", onInput (UpdateResponse i), value response, readonly obj.captured ] []
        , Button.button [ Button.secondary, Button.onClick (SendResponse i m.curr_section response), Button.disabled obj.captured ] [ text "Send" ]
        ]
    ]

