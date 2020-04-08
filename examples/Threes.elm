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
    board : List (List Int),
    size : Int,
    nextTile : Int
  }

init : (Model, Cmd Msg)

init = initializeBoard 4
    
initializeBoard size =
    let model = 
        repeat size <| repeat size 0
    in 
        (Model False model size 0, batch 
                        [ Random.generate (\(x,y) -> AddTile 1 x y) (Random.pair (Random.float 0 1) (Random.float 0 1))
                        , Random.generate (\(x,y) -> AddTile 2 x y) (Random.pair (Random.float 0 1) (Random.float 0 1)) ])


-- UPDATE

type Msg
  = Move Dir
  | AddTile Int Float Float
  | Restart
  | SetSize String

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
                , Random.generate (\(x,y) -> AddTile model.nextTile x y) (Random.pair (Random.float 0 1) (Random.float 0 1)))     

    AddTile tile x y ->
        if checkGameOver model.board then
            ({model | gameOver = True}, Cmd.none)
        else
            let pos = 1 + floor ((Debug.log (toString x) x) * (numZeroes model.board))
                board = 
                    recAddTileRow model.board pos tile
            in
                ({model | gameOver = checkGameOver board, board = board, nextTile = getNewTile y}, Cmd.none)

    Restart -> initializeBoard model.size

    SetSize str -> initializeBoard <| inputToSize str


doRight : List (List Int) -> List (List Int)
doRight = List.map mergeRight
doLeft = List.map mergeLeft
doUp board = transpose <| List.map mergeLeft <| transpose board
doDown board = transpose <| List.map mergeRight <| transpose board

checkGameOver board = 
    if numZeroes board > 0 then False
    else if (  (doRight board) == board 
            && (doLeft board) == board 
            && (doUp board) == board 
            && (doDown board) == board ) then True
    else False

numZeroes = sum << List.map (foldr (\x y -> if x == 0 then y + 1 else y) 0)

getNewTile : Float -> Int
getNewTile rand =
    if rand < 0.333 then 1 
    else if rand < 0.666 then 2
    else 3

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
            [] -> ([], n)
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
    let size = length board
        lst = recMerge {-<| removeZeroes-} board 
    in List.append lst (List.repeat (size - List.length lst) 0)

mergeRight : List Int -> List Int
mergeRight board = 
    let lst = mergeLeft <| List.reverse board
    in List.reverse lst 

removeZeroes : List Int -> List Int
removeZeroes board = 
    List.filter (\x -> x /= (log (toString board) 0)) board

recMerge : List Int -> List Int
recMerge lst = 
    case lst of
        [] -> []
        (x::xs) -> case xs of
                    [] -> [x]
                    (y::ys) ->  if y == 0 then x :: (recMerge xs)
                                else
                                    case x of
                                        0 -> y :: (recMerge (x :: ys))
                                        _ -> if y == 0 then x :: (recMerge xs)
                                            else if x + y == 3 then 3 :: (recMerge (0 :: ys))
                                            else if x == 1 || x == 2 then x :: (recMerge xs)
                                            else if x == y then (2*x) :: (recMerge (0 :: ys))
                                            else x :: (recMerge xs)

{-            case x of
                    0 -> case xs of
                        [] -> [x]
                        (y::ys) ->  if y == 0 then x :: (recMerge xs) 
                                    else y :: (recMerge (x :: ys))
                    _ -> case xs of
                        [] -> [x]
                        (y::ys) ->  if y == 0 then x :: (recMerge xs)
                                    else if x + y == 3 then 3 :: (recMerge ys)
                                    else if x == 1 || x == 2 then x :: (recMerge xs)
                                    else if x == y then (2*x) :: (recMerge ys)
                                    else x :: (recMerge xs)-}

inputToSize : String -> Int
inputToSize str =
    let i = String.toInt str
    in case i of
        Ok num -> if num > 0 && num < 6 then num else 4
        Err _ -> 4

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
  div [ style
        [ 
--          ("height", "500px") 
--        , ("width", "800px")
        -- , 
        ("margin", "10px")
        , ("font-family", 
            """
            "Clear Sans", "Helvetica Neue", Arial, sans-serif
            """) 
        ] 
    ]
    [ div 
        [ style 
            [ --("height", "500px")
--            , ("width", "500px")
            -- , 
            ("float", "left")
            , ("top","0"),("bottom","0"),("left","0"),("right","0")
            , ("position", "absolute")
            , ("margin", "inherit")
            , ("z-index", "-1")
            , ("font-family", 
                """
                "Clear Sans", "Helvetica Neue", Arial, sans-serif
                """)]
        ]
        [ viewTable model ]
    , div 
        [ style 
            [ --("height", "508px")
            --, ("width", "500px")
             -- , 
            ("background", "rgba(238, 228, 218, 0.73)")
            , ("float", "left")
            , ("top","0"),("bottom","0"),("left","0"),("right","0")
            , ("position", "absolute")
            , ("margin", "inherit")
            , ("display", if model.gameOver then "block" else "none")
            ]
        ]
        [ p
            [ style
                [ ("text-align", "center")
                , ("font-size", "60px")
                , ("margin-top", "30%")
                , ("margin-bottom", "3%")
                , ("font-weight", "bold")
                , ("color", "#776e65")
                ]
            ]
            [ text "Game over!" ]
        , button [ onClick Restart
                    , style 
                        [ ("background", "#8f7a66")
                        , ("border-radius", "3px")
                        , ("color", "#f9f6f2")
                        , ("height", "40px")
                        , ("padding", "0 20px")
                        , ("font-weight", "bold")
                        , ("font-size", "18px")
                        , ("outline", "0")
                        , ("border-radius", "3px")
                        , ("margin", "0 auto")
                        , ("display", "block")
                        ] 
                    ]
                [ text  "Restart" ]
        ]
    , div 
        [ style 
            [ -- ("height", "500px")
            -- , ("width", "300px")
            -- , 
            ("float", "right") ]
        ]
        [ div 
            [ style 
                [ ("background", "#bbada0")
                , ("padding", "15px 25px")
                , ("font-size", "22px")
                , ("height", "15px")
                , ("width", "87px")
                , ("font-weight", "bold")
                , ("border-radius", "3px")
                , ("color", "white")
                , ("margin-left", "20px")
                ]
            ]
            [ text "noscore"]
        , button [ onClick Restart
                    , style 
                        [ ("background", "#8f7a66")
                        , ("border-radius", "3px")
                        , ("color", "#f9f6f2")
                        , ("height", "40px")
                        , ("padding", "0 20px")
                        , ("font-weight", "bold")
                        , ("font-size", "18px")
                        , ("outline", "0")
                        , ("border-radius", "3px")
                        , ("margin-left", "20px")
                        , ("margin-top", "5px")
                        ] 
                    ]
                [ text  "New Game" ]
        , input [ onInput SetSize
                , placeholder "Set Size" 
                , style 
                    [ ("margin-left", "5px")
                    ]
                ]
            []
        ]
    ]

viewTable : Model -> Html Msg
viewTable model =
    table 
        [ style 
            [ 
            ---("height", "500px")
            -- , ("width", "500px")
            -- , 
            ("font-size", "45px")
            , ("border", "15px solid #bbada0")
            , ("border-collapse", "collapse")
            , ("background", "#bbada0")
            ]
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

        fontSize =
            if n > 500 then "40px"
            else "inherit"
    in 
        [ ("background", background)
        , ("color", color)
        , ("font-size", fontSize) ]

displayedText : Maybe Int -> String
displayedText maybe =
    case maybe of
        Just a -> toString a
        _ -> ""