import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

main = 
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    { topic : String
    , gifUrl : String
    , result : String
    }

init : (Model, Cmd Msg)
init =
    ( Model "cats" "waiting.gif" "", getRandomGif "cats")

-- UPDATE

type Msg = 
    MorePlease
    | ChangeTopic String
    | NewGif (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MorePlease ->
            (model, getRandomGif model.topic)
        ChangeTopic topic ->
            ({ model | topic = topic }, Cmd.none)
        NewGif (Ok newUrl) ->
            ({ model | gifUrl = newUrl }, Cmd.none)
        NewGif (Err _) ->
            ({ model | result = "An error occurred" }, Cmd.none)

getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++  topic
        request = Http.get url decodeGifUrl
    in
        Http.send NewGif request

decodeGifUrl : Decode.Decoder String
decodeGifUrl = Decode.at ["data", "image_url"] Decode.string  

-- VIEW

view : Model -> Html Msg

view model =
    div []
        [h2 [] [text model.topic]
        , img [src model.gifUrl] []
        , button [onClick MorePlease] [text "More please!"]
        , input [onInput ChangeTopic] []
        , h2 [] [text model.result]
        ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg

subscriptions model =
  Sub.none