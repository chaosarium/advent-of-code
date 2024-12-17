module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array
import Unwrap
import Bitwise

main : Program () Model Msg
main =
  Browser.sandbox { 
    init = init, 
    update = update, 
    view = view 
  }
  
type alias Model =
  { 
    input : String,
    output1 : String,
    output2 : String,
    trace : List ProgramState
  }

init : Model 
init = { input = "", output1 = "", output2 = "", trace = []}

type Instr = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv | Noop

type alias ProgramState = {
    a: Int,
    b: Int,
    c: Int,
    instrs: Array.Array Instr,
    counter: Int, 
    console: List Int
  }

-- parsing error handling is kind of bad :(
parseInput : String -> Result String ProgramState
parseInput input_str = 
  case String.split "\n" input_str of 
    lineA :: lineB :: lineC :: _ :: lineProg :: _ -> 
      let 
        getInit = String.split ":" >> Array.fromList >> Array.get 1 >> Unwrap.maybe >> String.trim >> String.toInt >> Unwrap.maybe
      in
      Ok {
        a = getInit lineA,
        b = getInit lineB,
        c = getInit lineC,
        instrs = String.split ":" lineProg |> Array.fromList |> Array.get 1 |> Unwrap.maybe |> String.trim |> String.split "," |> List.map (String.toInt >> Unwrap.maybe) |> List.map (\opCode -> case opCode of
          0 -> Adv
          1 -> Bxl
          2 -> Bst
          3 -> Jnz
          4 -> Bxc
          5 -> Out
          6 -> Bdv
          7 -> Cdv
          _ -> Noop -- unreachable
        ) |> Array.fromList,
        counter = 0,
        console = []
      }
    _ -> Err "cannot parse"

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
  Noop -> 8 -- unreachable

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
  Noop -> -1 -- unreachable

stepProgram : ProgramState -> Maybe ProgramState
stepProgram state = 
  case (Array.get state.counter state.instrs, Array.get (state.counter + 1) state.instrs) of 
    (Just instr, Just operand) -> 
      case instr of 
        Noop -> Just { state | counter = state.counter + 2 }
        Adv -> Just { state | counter = state.counter + 2, a = (state.a // (2 ^ comboOperand operand state))}
        Bxl -> Just { state | counter = state.counter + 2, b = Bitwise.xor state.b (literalOperand operand)}
        Bst -> Just { state | counter = state.counter + 2, b = modBy 8 (comboOperand operand state)}
        Jnz -> Just { state | counter = if state.a /= 0 then (literalOperand operand) else state.counter + 2}
        Bxc -> Just { state | counter = state.counter + 2, b = Bitwise.xor state.b state.c}
        Out -> Just { state | counter = state.counter + 2, console = (modBy 8 (comboOperand operand state))::state.console}
        Bdv -> Just { state | counter = state.counter + 2, b = (state.a // (2 ^ comboOperand operand state))}
        Cdv -> Just { state | counter = state.counter + 2, c = (state.a // (2 ^ comboOperand operand state))}
    _ -> Nothing

computePart1 : ProgramState -> (List Int, List ProgramState)
computePart1 state = case stepProgram state of 
  Nothing -> (List.reverse state.console, [state])
  Just stateNew -> 
    let
      _ = Debug.log "|->" stateNew
      (console, states) = computePart1 stateNew
    in 
      (console, state :: states)


type Msg 
  = SolvePart1 
  | SolvePart2
  | Input String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input s -> { model | input = s }
    SolvePart1 -> 
      let 
        program = parseInput model.input
        _ = Debug.log "program" program
        (consoleOutput, trace) = Debug.log "program |->*" (computePart1 (Unwrap.result program))
      in
        { model | output1 = String.join "," (List.map String.fromInt consoleOutput), trace = trace }
    SolvePart2 -> { model | output2 = "part 2 solution not implemented" }

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