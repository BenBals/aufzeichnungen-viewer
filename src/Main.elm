module Main exposing (..)

import ListContents
import Html.App as App


main =
    App.program
        { init = ListContents.init
        , view = ListContents.view
        , update = ListContents.update
        , subscriptions = ListContents.subscriptions
        }
