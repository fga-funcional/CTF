module Pages.CTF.Update exposing (..)

import Http
import Pages.CTF.Model exposing (..)
import Bootstrap.Alert as Alert
import Debug exposing (log)
import Url
import Browser
import Browser.Navigation as Nav

init : () -> Url.Url -> Nav.Key  -> ( Model, Cmd Msg )
init fs url key =
    ( Model url key [] 0 "" { alias = "", score = 0 } stdSection,  Http.send GotSectionsAPI getSections)

--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | Expand Int
    | ExpandFlag Section Int
    | UpdateResponse Int String
    | UpdatePlayerAlias String
    | SendResponse Int Section String
    -- | GetFlagsAPI 
    -- | GotFlagsAPI (Result Http.Error (List Flag))
    | GetSectionsAPI
    | GotSectionsAPI (Result Http.Error (List Section))
    | GetOneSectionAPI Int
    | GotOneSectionAPI (Result Http.Error Section)
    | SendPlayer
    | SentPlayer (Result Http.Error Player)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            (m, Cmd.none)
        -- GetFlagsAPI ->
        --     ( m, Http.send GotFlagsAPI getFlags )
        -- GotFlagsAPI result ->
        --     case result of
        --         Err httpError ->
        --             let
        --                 _ =
        --                     "FlagError"
        --             in
        --                 ( m, Cmd.none )

        --         Ok flags ->
        --             ( { m | flags = flags }, Cmd.none )
        UrlChanged url ->
             ({ m | curr_url = url}, Cmd.none)
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (m, Nav.pushUrl m.key (Url.toString url))
                Browser.External href ->
                    (m, Nav.load href)
        GetSectionsAPI ->
            ( m, Http.send GotSectionsAPI getSections )
        GotSectionsAPI result ->
            case result of
                Err httpError ->
                    let
                        _ =
                            "foo is"
                    in
                        ( m, Cmd.none )

                Ok sections ->
                    ( { m | sections = sections, curr_section = Maybe.withDefault stdSection (List.head sections) }, Cmd.none )
        GetOneSectionAPI id ->
            ( m, Http.send GotOneSectionAPI (getOneSection id))
        GotOneSectionAPI result ->
            case result of
                Err httpError ->
                    let
                        _ =
                            "foo is"
                    in
                        ( m, Cmd.none )

                Ok section ->
                    let
                        _ =
                            "foo is"
                        p = m.player
                    in
                        ( { m | curr_section = section, player = {p | score = 0} }, Cmd.none )

        Expand i ->
            ( { m | expanded = i }, Cmd.none )
        
        ExpandFlag sec i ->
            let
                cursec = m.curr_section
                cursecflags = cursec.flags
            in
                ({m | curr_section = {cursec | expanded = i, currentFlag = Maybe.withDefault stdFlag <| getElem (i+1) cursec.flags}  }, Cmd.none)
        
        UpdateResponse i st ->
            ( { m | response = st }, Cmd.none )

        UpdatePlayerAlias st ->
            let
                k = m.player
            in
            
            ( { m | player = { k | alias = st} }, Cmd.none )

        -- Pega flag de acordo com o valor de expanded da model. Pode da merda isso.
        SendResponse i sec ans ->
            let
                p =
                    m.player
                k = sec.flags
                a = Maybe.withDefault stdFlag <| List.head (List.filter(\x -> x.idFlag == i) k)
                cursec = m.curr_section
                flor = m.curr_section.currentFlag
                value = 
                    if ans == flor.answer then
                        flor.value
                    else
                        0
            in
            ( { m | response = "", player = { p | score = p.score + value}, 
                curr_section = { sec | flags = updateFlagList sec m.response (sec.expanded)}}, Cmd.none )
        SendPlayer ->
            ( m, savePlayerRequest m )
        SentPlayer result ->
            case result of
                Err httpError ->
                    let
                        _ =
                            "foo is"
                    in
                        ( m, Cmd.none )
                Ok player -> 
                    let
                        _ =
                            "foo is"
                    in
                        ( m, Cmd.none )
-- Atualiza lista de secoes com uma nova secao e ja ordena a nova lista de secoes
updateSectionList : List Section -> Section -> List Section 
updateSectionList sections curr_sec =
    let
        sec = 
            Maybe.withDefault stdSection <| List.head (List.filter(\m -> m.idSection == curr_sec.idSection) sections)
        sections_tmp =
            Maybe.withDefault [] <| List.tail <| List.reverse <| List.filter (\x -> x == sec) sections
    in
        List.sortBy .idSection <| sections_tmp ++ [sec]
        


updateFlagList : Section -> String -> Int -> List Flag
updateFlagList sec ans indexTo =
    let
        toggle index fla =
            if index == indexTo then
                let
                    answ = fla.answer 
                   
                    in   
                        if ans == answ then
                            {fla | color = "green", captured = True, alert = Alert.shown}
                        else    
                            {fla | color = "red", captured = False, alert = Alert.closed}
            else
                { fla | color = fla.color}
    in
        List.indexedMap toggle sec.flags

updateFlag : String -> Flag -> Flag
updateFlag ans f =   
     let
      answ = f.answer   
     in   
        if ans == answ then
            {f | color = "green"}
        else    
            {f | color = "red"}


getFlags : Http.Request (List Flag)
getFlags =
  Http.get "http://localhost:8080/api/flags" (flagDecoder)

getSections : Http.Request (List Section)
getSections =
  Http.get "http://localhost:8080/api/sections" (sectionDecoder)


getOneSection : Int -> Http.Request (Section)
getOneSection id = Http.get ("http://localhost:8080/api/sections/" ++ String.fromInt id) oneSectionDecoder

getOneFlag : Int -> Http.Request (Flag)
getOneFlag id = Http.get ("http://localhost:8080/api/flag/" ++ String.fromInt id) oneFlagDecoder


savePlayerRequest : Model -> Cmd Msg
savePlayerRequest model =
    let
        req = request model
    in
        Http.send SentPlayer req


request : Model -> Http.Request (Player)
request model =
    let
        headers =
            [ Http.header "Access-Control-Allow-Origin" ""
            , Http.header "Content-Type" "application/json"
            ]
    in
    Http.request { body = Http.jsonBody <| playerEncoder model  
        , expect = Http.expectJson playerDecoder
        , headers = headers
        , method = "POST"
        , timeout = Nothing
        , url = "http://localhost:8080/api/ranking"
        , withCredentials = False
        }  

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