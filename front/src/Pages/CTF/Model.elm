module Pages.CTF.Model exposing (..)

import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


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
    }


type alias Player =
    { score : Int
    }


{-| Create simple flag element
-}
flag : String -> Int -> String -> Bool -> String -> String -> Flag
flag color value title captured answer descr =
    Flag color value title False answer descr



----------------------------
-- Decoders
----------------------------

flagDecoder : D.Decoder (List Flag)
flagDecoder =
    D.list (D.map6 flag
        (D.at ["color"] D.string)
        (D.at ["value"] D.int)
        (D.at ["title"] D.string)
        (D.at ["captured"] D.bool)
        (D.at ["answer"] D.string)
        (D.at ["description"] D.string)
    )
