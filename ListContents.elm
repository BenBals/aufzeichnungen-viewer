module ListContents exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task
import Json.Decode as Json exposing ((:=), andThen)
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
    , viewLink : Maybe String
    }


type ElementKind
    = File
    | Directory
    | Other


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
            ( { model | contents = data, error = Nothing }, Cmd.none )

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
        , br [] []
        , div [] [
                 button [ onClick (Switch ("/")) ] [ text "Home" ]
                ]
        , div [] [ text (Maybe.withDefault "" model.error) ]
        ]


displayContent : Element -> Html Msg
displayContent element =
    div []
        [ if element.kind == Directory
          then button [ onClick (Switch ("/" ++ element.path)) ] [ text element.name ]
          else a [ href (Maybe.withDefault "" element.viewLink) ] [ text element.name ]
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
        Task.perform FailedContents ReceivedContents (Http.get decodeContentToElements url)


decodeContentToElement : Json.Decoder Element
decodeContentToElement =
    let
        stringToKind kind =
            case kind of
                "file" ->
                    File

                "dir" ->
                    Directory

                _ ->
                    Other

        decodeKind kind =
            Json.succeed (stringToKind kind)

        customKindDecoder =
            ("type" := Json.string) `andThen` decodeKind
    in
        Json.object4 Element
            ("name" := Json.string)
            ("path" := Json.string)
            customKindDecoder
            ("html_url" := Json.maybe Json.string)


decodeContentToElements : Json.Decoder (List Element)
decodeContentToElements =
    Json.list decodeContentToElement
