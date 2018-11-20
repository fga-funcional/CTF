module Pages.CTF.Main exposing (..)

import Browser


import Pages.CTF.View exposing (..)
import Pages.CTF.Model exposing (..)
import Pages.CTF.Update exposing (..)
import Pages.CTF.Subscription exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------


-- example : Model
-- example =
--     { init
--         | flags =
--             [ flag "Fibonacci" "Find some fibonacci numbers" 1 "white" False
--             , flag "Collatz" "Sequence of numbers" 2 "white" False
--             , flag "Factorial" "Blah" 2 "white" False
--             ]
--     }

