type move = Rock | Paper | Scissors
type movepair = move * move

let loses (m : move) : move =
  match m with Rock -> Scissors | Paper -> Rock | Scissors -> Paper

let wins (m : move) : move =
  match m with Rock -> Paper | Paper -> Scissors | Scissors -> Rock

let parse_move_char = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> failwith "invalid input"

let parse_move_naive (theirs : string) (ours : string) : movepair =
  (parse_move_char theirs, parse_move_char ours)

let parse_move (theirs : string) (ours : string) : movepair =
  let theirs = parse_move_char theirs in
  match ours with
  | "X" -> (theirs, loses theirs)
  | "Y" -> (theirs, theirs)
  | "Z" -> (theirs, wins theirs)
  | _ -> failwith "unknown move for us"

let score ((theirs, ours) : movepair) : int =
  let move_score = match ours with Rock -> 1 | Paper -> 2 | Scissors -> 3 in
  let win_score =
    match (theirs, ours) with
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> 6
    | theirs, ours when theirs == ours -> 3
    | _ -> 0
  in
  move_score + win_score

let () =
  Aoclib.aoc
    ~part1:(fun input ->
      input |> Aoclib.lines
      |> List.map (String.split_on_char ' ')
      |> List.map (function
           | [ theirs; ours ] -> parse_move_naive theirs ours
           | _ -> failwith "invalid input when mapping list")
      |> List.map score |> List.fold_left ( + ) 0)
    ~part2:(fun input ->
      input |> Aoclib.lines
      |> List.map (String.split_on_char ' ')
      |> List.map (function
           | [ theirs; ours ] -> parse_move theirs ours
           | _ -> failwith "invalid input when mapping list")
      |> List.map score |> List.fold_left ( + ) 0)
    2
