module Pages.CTF.Model exposing (..)

import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Bootstrap.Alert as Alert


--------------------------------------------------------------------------------
-- MODEL
--------------------------------------------------------------------------------


type alias Model =
    { sections : List Section
    , expanded : Int
    , response : String
    , player : Player
    , curr_section : Section
    }


type alias Flag =
    { idFlag : Int
    , color : String
    , value : Int
    , title : String
    , captured : Bool
    , answer : String
    , description : String
    , alert : Alert.Visibility
    }

type alias Section = 
    { idSection : Int
    , flags : List Flag
    , name : String   
    , expanded : Int
    }


type alias Player =
    { score : Int
    }


{-| Create simple flag element
-}
flag : Int -> String -> Int -> String -> Bool -> String -> String -> Alert.Visibility -> Flag
flag idFlag color value title captured answer descr alert =
    Flag idFlag color value title False answer descr Alert.closed

stdFlag =
    flag 999999999 "white" 0 "Inexistente" False "Nao existe" "Deu merda" Alert.closed

section : Int -> List Flag -> String -> Int -> Section
section idSection flags name expand =
    Section idSection flags name 0

stdSection =
    section 99999999 [stdFlag] "Secao inexistente" 0



----------------------------
-- Decoders
----------------------------
flagDecoder : D.Decoder (List Flag)
flagDecoder =
    D.list (
        oneFlagDecoder
    )

sectionDecoder : D.Decoder (List Section)
sectionDecoder = 
    D.list (
        oneSectionDecoder
    )


oneSectionDecoder : D.Decoder (Section)
oneSectionDecoder = 
    D.map4 section 
    (D.at ["idSection"] D.int)
    (D.at ["flags"] flagDecoder) 
    (D.at ["name"] D.string)
    (D.at [""] D.int)

oneFlagDecoder : D.Decoder (Flag)
oneFlagDecoder = 
    D.map8 flag 
    (D.at ["idFlag"] D.int)
    (D.at ["color"] D.string)
    (D.at ["value"] D.int)
    (D.at ["title"] D.string)
    (D.at ["captured"] D.bool)
    (D.at ["answer"] D.string)
    (D.at ["description"] D.string)
    (D.succeed Alert.closed)
    