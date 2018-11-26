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
    , alert : Alert.Visibility
    }

type alias Section = 
    { flags : List Flag
    , name : String    
    }


type alias Player =
    { score : Int
    }


{-| Create simple flag element
-}
flag : String -> Int -> String -> Bool -> String -> String -> Alert.Visibility -> Flag
flag color value title captured answer descr alert =
    Flag color value title False answer descr Alert.closed

section : List Flag -> String -> Section
section flags name =
    Section flags name



----------------------------
-- Decoders
----------------------------
flagDecoder : D.Decoder (List Flag)
flagDecoder =
    D.list (D.map7 flag
        (D.at ["color"] D.string)
        (D.at ["value"] D.int)
        (D.at ["title"] D.string)
        (D.at ["captured"] D.bool)
        (D.at ["answer"] D.string)
        (D.at ["description"] D.string)
        (D.succeed Alert.closed)
    )

sectionDecoder : D.Decoder (List Section)
sectionDecoder = 
    D.list (D.map2 section
    (D.at ["flags"] flagDecoder)
    (D.at ["name"] D.string)
    )