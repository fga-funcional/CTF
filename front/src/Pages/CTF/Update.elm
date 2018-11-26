module Pages.CTF.Update exposing (..)

import Http
import Pages.CTF.Model exposing (..)
import Bootstrap.Alert as Alert

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] 0 "" { score = 0 },  Http.send GotFlagsAPI getFlags)

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
                            {fla | color = "green", captured = True, alert = Alert.shown}
                        else    
                            {fla | color = "red", captured = False, alert = Alert.closed}
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


getFlags : Http.Request (List Flag)
getFlags =
  Http.get "http://localhost:3000/flags" (flagDecoder)


