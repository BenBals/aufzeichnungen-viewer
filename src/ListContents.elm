module ListContents exposing (..)

import Html exposing (..)
import Http
import Task
import Json.Decode as Json exposing ((:=))

-- MODEL

type alias Model =
  { contents : List String
  , error : Maybe String
  }

init : (Model, Cmd Msg)
init =
  ( Model [] Nothing
  , getContents
  )



-- UPDATE

type Msg
  = ReceivedContents (List String)
  | RequestContents
  | FailedContents Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedContents data ->
      ({ model | contents = data}, Cmd.none)
    RequestContents ->
      (model, getContents)
    FailedContents error ->
      ({ model | error = Just (toString error)}, Cmd.none)



-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ span [] [ text "Inhalte von haschis/aufzeichnungen"]
    , div [] (List.map displayContent model.contents)
    , div [] [ text (Maybe.withDefault "" model.error)]
    ]

displayContent : String -> Html Msg
displayContent name =
  div [] [ text name ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- HTTP
getContents : Cmd Msg
getContents =
  let
    url =
      "https://api.github.com/repos/haschis/aufzeichnungen/contents"
  in
    Task.perform FailedContents ReceivedContents (Http.get decodeContentNames url)

decodeContentNames : Json.Decoder (List String)
decodeContentNames =
  let
    decodeNameFromContent =
      "name" := Json.string
  in
    Json.list decodeNameFromContent
