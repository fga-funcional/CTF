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
    { color : String
    , value : Int 
    , idFlag : Int
    , title : String
    , answer : String
    , captured : Bool
    , description : String
    , alert : Alert.Visibility
    }

type alias Section = 
    { flags : List Flag
    , idSection : Int 
    , name : String   
    , expanded : Int
    }


type alias Player =
    { score : Int
    }


{-| Create simple flag element
-}
flag : String -> Int -> Int -> String -> String -> Bool -> String -> Alert.Visibility -> Flag
flag color value idFlag answer title captured  descr alert =
    Flag color value idFlag answer title False descr Alert.closed

stdFlag =
    flag "white" 0 999999999 "Inexistente" "Nao existe" False "Deu merda" Alert.closed

section : List Flag -> Int -> String -> Int -> Section
section flags idSection name expand =
    Section flags idSection name 0

stdSection =
    section [stdFlag] 99999999 "Secao inexistente" 0



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
    (D.at ["flags"] flagDecoder) 
    (D.at ["idSection"] D.int)
    (D.at ["name"] D.string)
    (D.succeed 0)

oneFlagDecoder : D.Decoder (Flag)
oneFlagDecoder = 
    D.map8 flag 
    (D.at ["color"] D.string)
    (D.at ["value"] D.int)
    (D.at ["idFlag"] D.int)
    (D.at ["answer"] D.string)
    (D.at ["title"] D.string)
    (D.at ["captured"] D.bool)
    (D.at ["description"] D.string)
    (D.succeed Alert.closed)
    