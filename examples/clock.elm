import Html exposing (Html) 
import Html.Events exposing (..)
import Svg exposing (..) 
import Svg.Attributes exposing (..) 
import Time exposing (Time, second)

main = 
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model = 
    { time : Time,
    paused : Bool }

init : (Model, Cmd Msg)
init =
    (Model 0 False, Cmd.none)

-- UPDATE

type Msg =
    Tick Time
    | Pause

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime -> 
            ({model | time = newTime}, Cmd.none)
        Pause ->
            ({model | paused = not model.paused}, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused
        then Sub.none
        else Time.every second Tick

-- VIEW

view : Model -> Html Msg
view model =
    let angle =
            turns (Time.inMinutes model.time)
        handX = 
            toString (50   +   40  *   cos angle)
        handY =
            toString (50 +   40  *   sin angle)
    in 
        Html.div []
            [svg [viewBox "0  0   100 100",   width   "300px" ]
                [circle  [cx  "50",cy  "50",r   "45",fill    "#0B79CE"   ][]
                ,line [x1  "50", y1  "50",   x2  handX,  y2  handY,  stroke  "#023963"][]
                ]
            , Html.button [onClick Pause] [Html.text (if model.paused then "Unpause" else "Pause")]
            ]

