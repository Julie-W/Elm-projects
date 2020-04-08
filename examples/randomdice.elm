import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import Mouse
import Keyboard exposing (..)
import Char
import String
import Array 
import List exposing (..)
import Debug exposing (..)
import Tuple exposing (..)
import Platform.Cmd exposing (..)

main =

  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =

  { gameOver : Bool,
    board : List (List Int)
  }

init : (Model, Cmd Msg)

init =
    let model = 
        repeat 4 <| repeat 4 0
    in 
        (Model False model, batch 
                        [ Random.generate (\(x,y) -> AddTile x y) (Random.pair (Random.int 1 (numZeroes model)) (Random.float 0 1))
                        , Random.generate (\(x,y) -> AddTile x y) (Random.pair (Random.int 1 ((numZeroes model)-1)) (Random.float 0 1)) ])

-- UPDATE

type Msg
  = Move Dir
  | AddTile Int Float
  | Restart

type Dir
    = Up 
    | Down
    | Left
    | Right
    | NoDir

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =

  case msg of

    Move dir ->
        let 
            newModel = 
                case dir of
                    Right -> doRight model.board
                    Left -> doLeft model.board
                    Up ->  doUp model.board
                    Down -> doDown model.board
                    NoDir -> model.board
        in
            if model.gameOver then (model, Cmd.none)
            else if newModel == model.board then ({model | board = newModel}, Cmd.none)
            else
                ( { model | board = newModel}
                , Random.generate (\(x,y) -> AddTile x y) (Random.pair (Random.int 1 (numZeroes newModel)) (Random.float 0 1)))     

    AddTile x y ->
        if checkGameOver model.board then
            ({model | gameOver = True}, Cmd.none)
        else
            let board = 
                recAddTileRow model.board (Debug.log (toString x) x) <| getNewTile y
            in
                ({model | gameOver = checkGameOver board, board = board}, Cmd.none)

    Restart -> init

checkGameOver board = 
    if numZeroes board > 0 then False
    else if (  (doRight board) == board 
            && (doLeft board) == board 
            && (doUp board) == board 
            && (doDown board) == board ) then True
    else False

doRight = List.map mergeRight
doLeft = List.map mergeLeft
doUp = transpose << List.map mergeLeft << transpose
doDown = transpose << List.map mergeRight << transpose

numZeroes = sum << List.map (foldr (\x y -> if x == 0 then y + 1 else y) 0)

getNewTile : Float -> Int
getNewTile rand =
    if rand < 0.9 then 2 else 4

recAddTileRow : List (List Int) -> Int -> Int -> List (List Int)
recAddTileRow lst n newTile =
    case lst of
        [] -> []
        (x::xs) ->  let (a,b) = 
                        recAddTile n newTile x
                    in a::(recAddTileRow xs b newTile)

recAddTile : Int -> Int -> List Int -> (List Int, Int)
recAddTile n newTile lst =
    if n > 0 then
        case lst of
            [] -> ([],(Debug.log (toString n) n))
            (x::xs) -> 
                if x == 0 then
                    if n == 1 then (newTile::xs,n-1)
                    else let rec = recAddTile (n-1) newTile xs in (x::(first rec), second rec)
                else
                    let rec = recAddTile n newTile xs in (x::(first rec), second rec)
    else
        (lst,n)

changeHead : List (List Int) -> List (List Int)
changeHead list = 
    case list of
        [] -> []
        (x::xs) -> case x of
            [] -> [[]]
            (y::ys) -> (2::ys)::xs 

transpose ll =
  case ll of
    [] ->
      []

    ([] :: xss) ->
      transpose xss

    ((x::xs) :: xss) ->
      let
        heads =
          List.filterMap List.head xss

        tails =
          List.filterMap List.tail xss
      in
        (x :: heads) :: transpose (xs :: tails)


mergeLeft : List Int -> List Int
mergeLeft board = 
    let lst = recMerge <| removeZeroes board 
    in List.append lst (List.repeat (4 - List.length lst) 0)

mergeRight : List Int -> List Int
mergeRight board = 
    List.reverse <| mergeLeft <| List.reverse board

removeZeroes : List Int -> List Int
removeZeroes board = 
    List.filter (\x -> x /= (log (toString board) 0)) board

recMerge : List Int -> List Int
recMerge lst = 
    case lst of
        [] -> []
        (x::xs) -> case xs of
                    [] -> [x]
                    (y::ys) -> if x == y then
                                (2*x) :: (recMerge ys)
                            else
                                x :: (recMerge xs)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg

subscriptions model =
  Sub.batch 
    [ downs (Move << detectDir) ]

detectDir : KeyCode -> Dir
detectDir keyCode =
    case keyCode of
        38 -> Up
        40 -> Down
        37 -> Left
        39 -> Right
        _ -> NoDir -- Change to maybe

-- VIEW

view : Model -> Html Msg

view model =
  div []
    [ div 
        [ style 
            [ ("height", "500px")
            , ("width", "500px")
            , ("background", "#bbada0")]
        ]
        [ viewTable model
        , h1 [] [text <| if model.gameOver then "GAMEOVER" else "Ok"] --text (toString (removeZeroes (fromList [2,0,2,4]))) ] --text (displayedText (get 0 model.board)) ]
        , button [onClick Restart] [ text "Restart" ]
        ]
    ]

viewTable : Model -> Html Msg
viewTable model =
    table 
        [ style 
            [ ("height", "500px")
            , ("width", "500px")
            , ("font-size", "55px")
            , ("border", "15px solid #bbada0")
            , ("border-collapse", "collapse")
            , ("font-family", 
                """
                "Clear Sans", "Helvetica Neue", Arial, sans-serif
                """)]
        ] (List.map viewRow model.board)

viewRow : List Int -> Html Msg
viewRow row =
    tr [] (List.map viewEntry row)

viewEntry : Int -> Html Msg
viewEntry entry =
    th [ style
            <| append  [ ("height", "107px")
                    , ("width", "90px")
                    , ("margin-right", "15px")
                    , ("border", "15px solid #bbada0")] (specificStyle entry)
        ] [ text <| if entry > 0 then (toString entry) else ""]

specificStyle n = 
    let background = 
            if n == 0 then "rgba(238, 228, 218, 0.35)"
            else if n == 2 then "#eee4da"
            else if n == 4 then "#ede0c8"
            else if n == 8 then "#f2b179"
            else if n == 16 then "#f59563"
            else if n == 32 then "#f67c5f"
            else if n == 64 then "#f65e3b"
            else if n == 128 then "#edcf72"
            else if n == 256 then "#edcc61"
            else if n == 512 then "#edc850"
            else if n == 1024 then "#edc53f"
            else "#edc22e" 
        
        color = 
            if n <= 4 then "#776e65"
            else "#f9f6f2"
    in 
        [ ("background", background)
        , ("color", color) ]

displayedText : Maybe Int -> String
displayedText maybe =
    case maybe of
        Just a -> toString a
        _ -> ""