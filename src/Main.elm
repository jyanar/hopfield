module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, table, tr, td)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Random


numRows : Int
numRows =
    4

numCols : Int
numCols =
    4

numUnits : Int
numUnits =
    numRows * numCols


type UnitState
    = On
    | Off


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


type alias Model =
    { state : List UnitState          -- The state of the Hopfield network
    , weights : List (List Int)       -- The weight matrix T
    , mems : List (List UnitState)    -- List of memories
    }

initialModel : () -> (Model, Cmd Msg)
initialModel _ =
    ( { state = List.repeat numUnits Off
      , weights = chunk numUnits (List.repeat (numUnits * numUnits) 0)
      , mems = []
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | Clear
    | Store
    | ToggleAt Int
    | UpdateAt Int
    | UpdateRequest


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

        UpdateRequest ->
            ( model
            , Random.generate UpdateAt (Random.int 0 (numUnits - 1))
            )


getAt : Int -> List a -> Maybe a
getAt idx list =
    if idx >= List.length list || idx < 0 then
        Maybe.Nothing
    else
        List.drop idx list |> List.head


flip : UnitState -> UnitState
flip b =
    if b == On then Off else On


toggleAt : Int -> List UnitState -> List UnitState
toggleAt idx state =
    List.indexedMap (\i b -> if i == idx then flip b else b) state


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
toBipolar : List UnitState -> List Int
toBipolar state =
    List.map (\u -> if u == On then 1 else -1) state

-- Switches representation from {+1, -1} to On/Off
toUnitState : List Int -> List UnitState
toUnitState state =
    List.map (\u -> if u == 1 then On else Off) state


{- Updates weight matrix T to store current network state
S via Hebbian rule:

    T = T + \sum_{i,j} S_i * S_j
-}
updateWeights : List (List Int) -> List UnitState -> List (List Int)
updateWeights weights state =
    let
        deltaWeights = outerProduct (toBipolar state) (toBipolar state)
        summedWeights = List.map2 (+) (List.concat weights) (List.concat deltaWeights)
    in
        setDiagZero (chunk numUnits summedWeights)


-- Replace the element at `idx` in a list with `newVal`.
replaceAt : Int -> a -> List a -> List a
replaceAt idx newVal =
    List.indexedMap (\i x -> if i == idx then newVal else x)


getSign : Int -> Int
getSign x =
    if x >= 0 then 1 else -1


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
            toBipolar state

        dotProduct : Int
        dotProduct =
            List.map2 (*) row stateBipolar |> List.sum

        newUnitState : UnitState
        newUnitState =
            dotProduct
                |> getSign
                |> (\u -> if u > 0 then On else Off)
    in
        -- replace only the `unit`th entry
        replaceAt unit newUnitState state


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
        , button [ onClick UpdateRequest ] [ text "Step"  ]
        , button [ onClick Clear ] [ text "Clear" ]
        , button [ onClick Store ] [ text "Store" ]
        , div [] [ text ("Memories: " ++ String.fromInt (List.length model.mems)) ]
        , viewMatrix model.weights
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
        [ style "width" cellSize
        , style "height" cellSize
        , style "border" "1px solid #ccc"
        , style "background-color" bgColor
        , style "cursor" "pointer"
        , onClick (ToggleAt idx)
        ]
        []



viewGrid : List UnitState -> Html Msg
viewGrid state =
    state
        |> List.indexedMap viewCell
        |> chunk numCols
        |> List.map (\row ->
            div [ style "display" "flex" ] row
           )
        |> div []

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


