module ListContents exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task
import Json.Decode as Json exposing ((:=))
import String


-- MODEL


type alias Model =
    { contents : List Element
    , input : String
    , path : String
    , error : Maybe String
    }


type alias Element =
    { name : String
    , path : String
    , kind : ElementKind
    , downloadLink : Maybe String
    }


type ElementKind
    = File
    | Directory


init : ( Model, Cmd Msg )
init =
    ( Model [] "/" "/" Nothing
    , getContents "/"
    )



-- UPDATE


type Msg
    = ReceivedContents (List Element)
    | RequestContents
    | FailedContents Http.Error
    | Input String
    | Switch String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedContents data ->
            ( { model | contents = data }, Cmd.none )

        RequestContents ->
            ( { model | path = model.input }, getContents model.input )

        FailedContents error ->
            ( { model | error = Just (toString error) }, Cmd.none )

        Input input ->
            ( { model | input = input }, Cmd.none )

        Switch path ->
            ( { model | input = path, path = path }, getContents path )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ span [] [ text "Inhalte von haschis/aufzeichnungen" ]
        , div []
            [ input [ onInput Input, value model.input ] []
            , button [ onClick RequestContents ] [ text "Laden" ]
            ]
        , div [] (List.map displayContent model.contents)
        , div [] [ text (Maybe.withDefault "" model.error) ]
        ]


displayContent : Element -> Html Msg
displayContent element =
    div []
        [ button [ onClick (Switch ("/" ++ element.path)) ] [ text content.name ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getContents : String -> Cmd Msg
getContents path =
    let
        url =
            "https://api.github.com/repositories/55137831/contents/" ++ (String.dropLeft 1 path)
    in
        Task.perform FailedContents ReceivedContents (Http.get decodeContentNames url)


decodeContentToElement : Json.Decoder Element
decodeContentToElement = 


decodeContentNames : Json.Decoder (List Element)
decodeContentNames =
    Json.list decodeContentToElement
