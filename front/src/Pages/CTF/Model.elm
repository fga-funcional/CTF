module Pages.CTF.Model exposing (..)

import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Bootstrap.Alert as Alert
import Browser.Navigation as Nav
import Url


--------------------------------------------------------------------------------
-- MODEL
--------------------------------------------------------------------------------


type alias Model =
    { curr_url : Url.Url
    , key : Nav.Key
    , sections : List Section
    , expanded : Int
    , response : String
    , player : Player
    , curr_section : Section
    , ranking : List Player 
    , showRank : String
    , showFlags : String
    }


type alias Flag =
    { color : String
    , value : Int 
    , idFlag : Int
    , answer : String
    , title : String
    , captured : Bool
    , description : String
    , alert : Alert.Visibility
    }

type alias Section = 
    { flags : List Flag
    , name : String   
    , idSection : Int 
    , expanded : Int
    , currentFlag : Flag
    }


type alias Player =
    { 
        alias : String,
        score : Int
    }


{-| Create simple flag element
-}
flag : String -> Int -> Int -> String -> String -> Bool -> String -> Alert.Visibility -> Flag
flag color value idFlag answer title captured  descr alert =
    Flag color value idFlag answer title False descr Alert.closed

stdFlag =
    flag "white" 0 999999999 "Inexistente" "Nao existe" False "Deu merda" Alert.closed

section : List Flag ->  String -> Int -> Int -> Flag -> Section
section flags name idSection expand flage =
    Section flags name idSection -1 flage

stdSection =
    section [] "Secao inexistente"  99999999 -1 stdFlag

player : String -> Int -> Player
player aliasPlayer points = 
    Player aliasPlayer points


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
    D.map5 section 
    (D.at ["flags"] flagDecoder) 
    (D.at ["name"] D.string)
    (D.at ["idSection"] D.int)
    (D.succeed -1)
    (D.succeed stdFlag)

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

allPlayerDecoder : D.Decoder (List Player)
allPlayerDecoder =
    D.list (playerDecoder)

playerDecoder : D.Decoder (Player)
playerDecoder =
    D.map2 player
        (D.at ["aliasPlayer"] D.string)
        (D.at ["points"] D.int)

playerEncoder : Model -> E.Value
playerEncoder m =
    E.object
        [ ( "aliasPlayer", E.string m.player.alias )
        , ( "points", E.int m.player.score )
        ]