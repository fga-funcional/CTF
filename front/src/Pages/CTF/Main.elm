module Pages.CTF.Main exposing (main)

import Browser
import Pages.CTF.Model exposing (..)
import Pages.CTF.Subscription exposing (..)
import Pages.CTF.Update exposing (..)
import Pages.CTF.View exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
