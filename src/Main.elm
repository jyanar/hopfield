module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, table, tr, td)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick, onMouseUp, onMouseDown, onMouseEnter)

import Random
import Time
import Platform.Cmd as Cmd


numRows : Int
numRows =
    28

numCols : Int
numCols =
    28

numUnits : Int
numUnits =
    numRows * numCols


type UnitState
    = On
    | Off


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { state : List UnitState          -- The state of the Hopfield network
    , weights : List (List Int)       -- The weight matrix T
    , mems : List (List UnitState)    -- List of memories
    , playing : Bool                  -- Whether to simulate the network forward
    , drawing : Bool                  -- Whether user is currently drawing
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { state = List.repeat numUnits Off
      , weights = zeroMatrix numUnits
      , mems = []
      , playing = False
      , drawing = False
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | Clear
    | Store
    | ToggleAt Int
    | UpdateAt Int
    | UpdateRandomUnit
    | Play
    | Pause
    | Xor
    | StartDrawing Int
    | DrawAt Int
    | StopDrawing
    | FlipAt (List Int)
    | FlipNBits
    | SyncStep
    | Stamp Int    -- Stamp memory i into the network state
    | ClearMemories


tickDt : Float
tickDt =
    17.0

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing then
        Time.every tickDt (\_ -> UpdateRandomUnit)
    else
        Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        Clear ->
            ({ model | state = List.repeat numUnits Off }
            , Cmd.none
            )

        Store ->
            ({ model | mems = model.state :: model.mems                    -- Add memory to list of memories
                     , weights = updateWeights model.weights model.state } -- Update weight matrix
            , Cmd.none
            )

        ToggleAt idx ->
            ({ model | state = toggleAt idx model.state }
            , Cmd.none
            )

        UpdateAt idx ->
            ({ model | state = updateAsync idx model.weights model.state }
            , Cmd.none
            )

        UpdateRandomUnit ->
            ( model
            , Random.generate UpdateAt (Random.int 0 (numUnits - 1))
            )

        FlipAt idxs ->
            ({ model | state = (flipBits idxs model.state) }
            , Cmd.none
            )

        FlipNBits ->
            ( model
            , Random.generate FlipAt (Random.list 30 (Random.int 0 (numUnits - 1)))
            )

        Play ->
            ({ model | playing = True }
            , Cmd.none
            )

        Pause ->
            ({ model | playing = False }
            , Cmd.none
            )

        Xor ->
            ({ model | state = List.map flip model.state}
            , Cmd.none
            )

        StartDrawing idx ->
            ({ model | state = (turnOnAt idx model.state), drawing = True }
            , Cmd.none
            )

        DrawAt idx ->
            if model.drawing then
                ({ model | state = turnOnAt idx model.state }
                , Cmd.none
                )
            else
                (model , Cmd.none)

        StopDrawing ->
            ({ model | drawing = False }
            , Cmd.none
            )

        SyncStep ->
            ({ model | state = updateSync model.weights model.state }
            , Cmd.none
            )

        Stamp idx ->
            let
                mem = Maybe.withDefault
                        (List.repeat numUnits Off)
                        (getAt idx model.mems)
            in
            ({ model | state = stampMemory mem model.state }
            , Cmd.none
            )

        ClearMemories ->
            ({ model | mems = []
                     , weights = zeroMatrix numUnits }
            , Cmd.none
            )


zeroMatrix : Int -> List (List Int)
zeroMatrix n =
    chunk n (List.repeat (n * n) 0)


getAt : Int -> List a -> Maybe a
getAt idx list =
    if idx >= List.length list || idx < 0 then
        Maybe.Nothing
    else
        List.drop idx list |> List.head


flip : UnitState -> UnitState
flip b =
    if b == On then Off else On

flipBits : List Int -> List UnitState -> List UnitState
flipBits idxs state =
    List.indexedMap (\i b -> if (List.member i idxs) then flip b else b) state

toggleAt : Int -> List UnitState -> List UnitState
toggleAt idx state =
    List.indexedMap (\i b -> if i == idx then flip b else b) state

turnOnAt : Int -> List UnitState -> List UnitState
turnOnAt idx state =
    List.indexedMap (\i b -> if i == idx then On else b) state


outerProduct : List Int -> List Int -> List (List Int)
outerProduct xs ys =
    List.map (\x -> List.map (\y -> x * y ) ys) xs


setDiagZero : List (List Int) -> List (List Int)
setDiagZero m =
    m |> List.indexedMap
        (\rowIndex row ->
            row |> List.indexedMap
                (\colIndex val ->
                    if rowIndex == colIndex then
                        0
                    else
                        val
                )
        )


-- Switches representation from On/Off to {+1, -1}
toBipolar : UnitState -> Int
toBipolar s =
    if s == On then 1 else -1

-- Switches representation from {+1, -1} to On/Off
toUnitState : Int -> UnitState
toUnitState i =
    if i == 1 then On else Off


-- Replace the element at `idx` in a list with `newVal`.
replaceAt : Int -> a -> List a -> List a
replaceAt idx newVal =
    List.indexedMap (\i x -> if i == idx then newVal else x)


getSign : Int -> Int
getSign x =
    if x >= 0 then 1 else -1


dotProduct : List number -> List number -> number
dotProduct xs ys =
    List.map2 (*) xs ys |> List.sum


{- Updates weight matrix T to store current network state
S via Hebbian rule:

    T = T + \sum_{i,j} S_i * S_j
-}
updateWeights : List (List Int) -> List UnitState -> List (List Int)
updateWeights weights state =
    let
        deltaWeights = outerProduct (List.map toBipolar state) (List.map toBipolar state)
        summedWeights = List.map2 (+) (List.concat weights) (List.concat deltaWeights)
    in
    setDiagZero (chunk numUnits summedWeights)


{-| Given
    1) the index of the neuron to update
    2) the current weight‐matrix
    3) the current state

    returns a new state list in which only `unit` has been
    recomputed as

        newState = sign( dot(row, state) )
-}
updateAsync : Int -> List (List Int) -> List UnitState -> List UnitState
updateAsync unit weights state =
    let
        row : List Int
        row =
            Maybe.withDefault [] (getAt unit weights)

        stateBipolar : List Int
        stateBipolar =
            List.map toBipolar state

        newUnitState : UnitState
        newUnitState =
            dotProduct row stateBipolar
                |> getSign
                |> toUnitState
    in
    replaceAt unit newUnitState state


{- Simultaneously updates all units in the network.  -}
updateSync : List (List Int) -> List UnitState -> List UnitState
updateSync weights state =
    let
        stateBipolar : List Int
        stateBipolar =
            List.map toBipolar state

        newStateForRow : List Int -> UnitState
        newStateForRow row =
            dotProduct row stateBipolar
                |> getSign
                |> toUnitState
    in
    List.map newStateForRow weights


stampMemory : List UnitState -> List UnitState -> List UnitState
stampMemory mem state =
    List.map2 (\m s -> if m == On then On else s) mem state


-- Chunk a list into sublists of length `n`.
chunk : Int -> List a -> List (List a)
chunk n xs =
    if n <= 0 then
        []
    else
        case xs of
            [] ->
                []

            _ ->
                let
                    h = List.take n xs
                    t = List.drop n xs
                in
                h :: chunk n t


-- RENDERING


cellSize : String
cellSize =
    "10px"


view : Model -> Html Msg
view model =
    div []
        [ viewGrid model.state
        , button [ onClick UpdateRandomUnit ] [ text "Step" ]
        , button [ onClick Clear ] [ text "Clear" ]
        , button [ onClick Store ] [ text "Store" ]
        , button [ onClick Play  ] [ text "Play"  ]
        , button [ onClick Pause ] [ text "Pause" ]
        , button [ onClick Xor   ] [ text "XOR"   ]
        , button [ onClick FlipNBits ] [ text "FlipBits" ]
        , button [ onClick SyncStep ] [ text "SyncStep" ]
        , button [ onClick ClearMemories ] [ text "Clear Memories" ]
        , div [] [ text ("Memories: " ++ String.fromInt (List.length model.mems)) ]
        , viewMemories model.mems
        -- , viewMatrix model.weights
        -- , div [] [ text ("Length of state: " ++ String.fromInt (List.length model.state))]
        -- , div [] [ text ("Length of mem[1]: " ++ String.fromInt (List.length (Maybe.withDefault [] (List.head model.mems))))]
        -- , div [] [ text ("Mem 1: " ++ (model.mems
        --     |> List.head
        --     |> Maybe.withDefault []
        --     |> toBipolar
        --     |> List.map String.fromInt
        --     |> String.concat))]
        ]


viewCell : Int -> UnitState -> Html Msg
viewCell idx val =
    let
        bgColor = if val == On then "#000" else "#fff"
    in
    div
        [ attribute "draggable" "false"
        , style "user-select" "none"
        , style "width" cellSize
        , style "height" cellSize
        , style "border" "1px solid #ccc"
        , style "background-color" bgColor
        -- , style "cursor" "pointer"
        -- , onClick (ToggleAt idx)
        , onMouseDown (StartDrawing idx)
        , onMouseEnter (DrawAt idx)
        , onMouseUp StopDrawing
        ]
        []



viewGrid : List UnitState -> Html Msg
viewGrid state =
    div [ onMouseUp StopDrawing ]
        ( state
            |> List.indexedMap viewCell
            |> chunk numCols
            |> List.map (\row -> div [ style "display" "flex" ] row)
        )

-- | Turn a List (List Int) into a styled HTML table
viewMatrix : List (List Int) -> Html msg
viewMatrix matrix =
    table
        [ style "border-collapse" "collapse"
        , style "margin" "1em 0"
        ]
        (List.map viewRow matrix)


-- | Render one row of Ints as a <tr> of <td>s
viewRow : List Int -> Html msg
viewRow row =
    tr []
        (List.map viewMatCell row)


-- | Render one Int as a styled <td>
viewMatCell : Int -> Html msg
viewMatCell n =
    td
        [ style "border" "1px solid #ccc"
        , style "padding" "4px 8px"
        , style "text-align" "right"
        ]
        [ text (String.fromInt n) ]


-- | Render the list of memories side by side
viewMemories : List (List UnitState) -> Html Msg
viewMemories mems =
    div
      [ style "display" "flex"
      , style "gap" "8px"
      , style "margin-top" "1em"
      ]
      (List.indexedMap viewMemory mems)


-- | Render one memory (a List of UnitState) as a tiny grid
viewMemory : Int -> List UnitState -> Html Msg
viewMemory idx mem =
    let
        thumbSize =
            "3px"
        rows =
            chunk numCols mem

        rowView : List UnitState -> Html Msg
        rowView rowStates =
            div [ style "display" "flex" ]
                (List.map (viewMemoryCell thumbSize) rowStates)
    in
    div
      [ style "border" "1px solid #ddd"
      , style "padding" "2px"
      , style "background" "#f9f9f9"
      , onClick (Stamp idx)
      ]
      (List.map rowView rows)


-- | Render one “pixel” of a memory thumbnail
viewMemoryCell : String -> UnitState -> Html Msg
viewMemoryCell size state =
    let
        bg =
            case state of
              On  -> "#000"
              Off -> "#fff"
    in
    div
      [ style "width" size
      , style "height" size
      , style "background-color" bg
    --   , style "border" "1px solid #ccc"
      ]
      []