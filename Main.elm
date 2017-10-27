module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)
import Task exposing (andThen, perform)
import Http
import Json.Decode as Json


-- INIT


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Topic
    = Cats
    | Dogs
    | Dan
    | Other String


type alias Model =
    { topic : Topic
    , gifUrl : String
    , error : Maybe Http.Error
    , requestState : RequestState
    , showFreeText : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model Cats "" Nothing Initial False, Cmd.none )



-- UPDATE


type RequestState
    = Initial
    | InProgress Time
    | NotFound
    | Complete Time


type Msg
    = SetTopic Topic
    | MorePlease
    | SetStartTime Time
    | ReceiveResponse (Result Http.Error (Maybe String))
    | GetEndTime
    | SetEndTime Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTopic topic ->
            ( { model | topic = topic }, Cmd.none )

        MorePlease ->
            ( { model | requestState = InProgress 0 }, perform SetStartTime Time.now )

        SetStartTime time ->
            ( { model | gifUrl = "", requestState = InProgress time }, getRandomGif model.topic )

        ReceiveResponse (Ok (Just newUrl)) ->
            ( { model | gifUrl = newUrl }, Cmd.none )

        ReceiveResponse (Ok Nothing) ->
            ( { model | gifUrl = "", requestState = NotFound }, Cmd.none )

        ReceiveResponse (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        GetEndTime ->
            ( model, perform SetEndTime Time.now )

        SetEndTime time ->
            let
                startedAt =
                    case model.requestState of
                        InProgress time ->
                            time

                        _ ->
                            0

                duration =
                    time - startedAt
            in
                ( { model | requestState = Complete duration }, Cmd.none )


getRandomGif : Topic -> Cmd Msg
getRandomGif topic =
    let
        value =
            case topic of
                Cats ->
                    "cats"

                Dogs ->
                    "dogs"

                Dan ->
                    "dan"

                Other name ->
                    name

        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ value

        request =
            Http.get url decodeGifUrl
    in
        Http.send ReceiveResponse request


decodeGifUrl : Json.Decoder (Maybe String)
decodeGifUrl =
    Json.at [ "data", "image_url" ] Json.string
        |> Json.maybe



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


onLoad : Msg -> Attribute Msg
onLoad message =
    on "load" (Json.succeed message)


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Request a Random Image" ]
        , div [] [ viewTopicSelection model.showFreeText model.topic ]
        , div []
            [ button [ onClick MorePlease ] [ text "More Please!" ]
            ]
        , div []
            [ img [ src model.gifUrl, onLoad GetEndTime ] []
            , div [] [ viewDuration model.requestState ]
            , viewError model.error
            ]
        ]


viewError : Maybe Http.Error -> Html Msg
viewError error =
    case error of
        Just (Http.BadUrl url) ->
            text ("Bad URL " ++ url)

        Just Http.Timeout ->
            text "Network timeout"

        Just Http.NetworkError ->
            text "No network"

        Just (Http.BadStatus resp) ->
            text ("Bad response. Code = " ++ (toString resp.status.code))

        Just (Http.BadPayload message _) ->
            text ("Bad payload " ++ message)

        Nothing ->
            text ""


viewDuration : RequestState -> Html Msg
viewDuration requestState =
    case requestState of
        Initial ->
            text "Not Started"

        InProgress _ ->
            text "Fetching an image"

        NotFound ->
            text "Image not found: Try again."

        Complete duration ->
            let
                message =
                    "It took " ++ (toString (Time.inSeconds duration)) ++ " seconds"
            in
                text message


viewTopicSelection : Bool -> Topic -> Html Msg
viewTopicSelection showFreeText currentTopic =
    let
        isChecked topic =
            case topic of
                Cats ->
                    currentTopic == Cats

                Dogs ->
                    currentTopic == Dogs

                Dan ->
                    currentTopic == Dan

                Other _ ->
                    case currentTopic of
                        Other _ ->
                            True

                        _ ->
                            False
    in
        div
            []
            [ input
                [ id "radio-cats", type_ "radio", checked (isChecked Cats), onCheck (always (SetTopic Cats)) ]
                []
            , label [ for "radio-cats" ] [ text "Cats" ]
            , input
                [ id "radio-dogs", type_ "radio", checked (isChecked Dogs), onCheck (always (SetTopic Dogs)) ]
                []
            , label [ for "radio-dogs" ] [ text "Dogs" ]
            , input
                [ id "radio-dan", type_ "radio", checked (isChecked Dan), onCheck (always (SetTopic Dan)) ]
                []
            , label [ for "radio-dan" ] [ text "Dan" ]
            , input
                [ id "radio-other", type_ "radio", checked (isChecked (Other "")), onCheck (always (SetTopic (Other ""))) ]
                []
            , label
                [ for "radio-other" ]
                [ text "Other"
                , input
                    [ type_ "text", placeholder "Topic", onInput (\value -> SetTopic (Other value)) ]
                    []
                ]
            ]
