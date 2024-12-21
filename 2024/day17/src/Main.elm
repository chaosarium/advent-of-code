module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array
import Bitwise

main : Program () Model Msg
main =
  Browser.sandbox { 
    init = init, 
    view = view,
    update = update
  }
  
type alias Model =
  { 
    input : String,
    output1 : String,
    output2 : String,
    trace : List ProgramState
  }

init : Model 
init = { input = "", output1 = "", output2 = "", trace = [] }

type Instr = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv

type alias ProgramState = {
    a: Int,
    b: Int,
    c: Int,
    instrs: Array.Array Instr,
    counter: Int, 
    console: List Int
  }

maybeListMap : (a -> Maybe b) -> List a -> Maybe (List b)
maybeListMap f l =
  List.foldr (\x acc -> case (f x, acc) of
    (Just fx, Just fxs) -> Just (fx :: fxs)
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
  ) (Just []) l

unnestMaybe : Maybe (Maybe a) -> Maybe a
unnestMaybe mma = 
  case mma of 
    Just ma -> ma
    Nothing -> Nothing

parseInitValue : String -> Maybe Int
parseInitValue line =
  String.split ":" line
    |> Array.fromList
    |> Array.get 1
    |> Maybe.map String.trim
    |> Maybe.andThen String.toInt

opcodeToInstr : Int -> Maybe Instr
opcodeToInstr opCode =
  case opCode of
    0 -> Just Adv
    1 -> Just Bxl
    2 -> Just Bst
    3 -> Just Jnz
    4 -> Just Bxc
    5 -> Just Out
    6 -> Just Bdv
    7 -> Just Cdv
    _ -> Nothing

parseInstructions : String -> Maybe (Array.Array Instr)
parseInstructions lineProg =
  String.split ":" lineProg
    |> Array.fromList
    |> Array.get 1
    |> Maybe.map String.trim
    |> Maybe.map (String.split ",")
    |> Maybe.map (maybeListMap String.toInt)
    |> unnestMaybe
    |> Maybe.map (maybeListMap opcodeToInstr)
    |> unnestMaybe
    |> Maybe.map Array.fromList

parseInput : String -> Result String ProgramState
parseInput inputStr =
  case String.split "\n" inputStr of
    lineA :: lineB :: lineC :: _ :: lineProg :: _ -> 
      Maybe.map4 ProgramState
        (parseInitValue lineA)
        (parseInitValue lineB)
        (parseInitValue lineC)
        (parseInstructions lineProg)
      |> Maybe.andThen (\x -> Just (x 0))
      |> Maybe.andThen (\x -> Just (x []))
      |> Result.fromMaybe "failed to parse input"
    _ ->
      Err "failed to parse input"

literalOperand : Instr -> Int
literalOperand instr = case instr of 
  Adv -> 0
  Bxl -> 1
  Bst -> 2
  Jnz -> 3
  Bxc -> 4
  Out -> 5
  Bdv -> 6
  Cdv -> 7

comboOperand : Instr -> ProgramState -> Int
comboOperand instr state = case instr of 
  Adv -> 0
  Bxl -> 1
  Bst -> 2
  Jnz -> 3
  Bxc -> state.a
  Out -> state.b
  Bdv -> state.c
  Cdv -> -1 -- unreachable

stepProgram : ProgramState -> Maybe ProgramState
stepProgram stateOrig = 
  case (Array.get stateOrig.counter stateOrig.instrs, Array.get (stateOrig.counter + 1) stateOrig.instrs) of 
    (Just instr, Just operand) -> 
      let state = { stateOrig | counter = stateOrig.counter + 2 } in 
      case instr of 
        -- Noop -> Just state
        Adv -> Just { state | a = (state.a // (2 ^ comboOperand operand state))}
        Bxl -> Just { state | b = Bitwise.xor state.b (literalOperand operand)}
        Bst -> Just { state | b = modBy 8 (comboOperand operand state)}
        Jnz -> Just (if state.a /= 0 then { state | counter = literalOperand operand } else state)
        Bxc -> Just { state | b = Bitwise.xor state.b state.c}
        Out -> Just { state | console = (modBy 8 (comboOperand operand state))::state.console}
        Bdv -> Just { state | b = (state.a // (2 ^ comboOperand operand state))}
        Cdv -> Just { state | c = (state.a // (2 ^ comboOperand operand state))}
    _ -> Nothing

traceProgram : ProgramState -> (List Int, List ProgramState)
traceProgram state = case stepProgram state of 
  Nothing -> (List.reverse state.console, [state])
  Just stateNew -> 
    let
      _ = Debug.log "|->" stateNew
      (console, states) = traceProgram stateNew
    in 
      (console, state :: states)

runProgram : ProgramState -> Int -> List Int
runProgram state maxSteps = 
  if maxSteps == 0 then [] else 
  case stepProgram state of 
    Nothing -> (List.reverse state.console)
    Just stateNew -> runProgram stateNew (maxSteps - 1)

type Msg 
  = SolvePart1 
  | SolvePart2
  | Input String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input s -> { model | input = s }
    SolvePart1 -> 
      case parseInput model.input of 
        Err e -> { model | output1 = e }
        Ok program -> let (consoleOutput, trace) = Debug.log "program |->*" (traceProgram program) in
          { model | output1 = String.join "," (List.map String.fromInt consoleOutput), trace = trace }
    SolvePart2 -> 
      case parseInput model.input of 
        Err e -> { model | output2 = e }
        Ok _ -> { model | output2 = "somehow my elm implementation overflows for part 2 T^T, but this can actually be solved by hand :)"}

renderTrace : List ProgramState -> Html msg
renderTrace states = 
  let 
    renderRow state = tr [] [
        td [] [ text (String.fromInt state.a) ],
        td [] [ text (String.fromInt state.b) ],
        td [] [ text (String.fromInt state.c) ],
        td [] (
            List.indexedMap (\i -> \x ->
                let
                  opcode = x |> literalOperand 
                  inner = span [] [ text (String.fromInt opcode ++ " ")]
                  content = 
                    if i == state.counter 
                    then strong [ style "color" "red" ] [inner] 
                    else inner
                in content
              ) (Array.toList state.instrs)
          )
      ]
    rows = states |> List.map renderRow
    tableHead = tr [] [
        th [] [text "Register A"],
        th [] [text "Register B"],
        th [] [text "Register C"],
        th [] [text "Program"]
      ] 
  in
    tableHead :: rows |> table []

view : Model -> Html Msg
view model =
  div []
    [
      div [] [
        textarea [ onInput Input, value model.input, cols 40, rows 6, placeholder "paste input here" ] [  ]
      ],
      button [ onClick SolvePart1 ] [ text "compute part 1" ],
      button [ onClick SolvePart2 ] [ text "compute part 2" ],
      br [] [],
      div [ style "margin-top" "10px" ] [
        text "part 1 output:",
        br [] [],
        textarea [ disabled True, value model.output1, cols 40, rows 3, placeholder "nothing computed yet" ] []
      ],
      div [ style "margin-top" "10px" ] [
        text "part 2 output:",
        br [] [],
        textarea [ disabled True, value model.output2, cols 40, rows 3, placeholder "nothing computed yet" ] []
      ],
      div [ style "margin-top" "10px" ] [
        renderTrace model.trace
      ]
    ]