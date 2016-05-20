module Main exposing (..)

import ListContents
import Html


main =
    Html.program
        { init = ListContents.init
        , view = ListContents.view
        , update = ListContents.update
        , subscriptions = ListContents.subscriptions
        }
