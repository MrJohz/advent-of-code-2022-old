type action = Tests | Part1Demo | Part1 | Part2Demo | Part2

let action_of_str (str : string) : action =
  match str with
  | "tests" -> Tests
  | "part1demo" -> Part1Demo
  | "part1" -> Part1
  | "part2demo" -> Part2Demo
  | "part2" -> Part2
  | _ -> failwith "cannot reach here"

let open_file (filename : string) : string =
  In_channel.with_open_bin filename In_channel.input_all

let aoc ?(tests : OUnit2.test list = [])
    ?(part1 : string -> string = fun _ -> failwith "not implemented")
    ?(part2 : string -> string = fun _ -> failwith "not implemented")
    (day : int) =
  let do_action (action : action) =
    print_newline ();
    match action with
    | Tests ->
        Printf.printf "Running tests for day %d:\n" day;
        OUnit2.run_test_tt_main
          (OUnit2.( >::: ) (Printf.sprintf "Tests for day %d" day) tests)
    | Part1Demo ->
        Printf.printf "Running part 1 against demo input for day %d:\n" day;
        Printf.printf "%s\n"
          (part1 (open_file (Printf.sprintf "inputs/day_%d_demo.txt" day)))
    | Part1 ->
        Printf.printf "Running part 1 against real input for day %d:\n" day;
        Printf.printf "%s\n"
          (part1 (open_file (Printf.sprintf "inputs/day_%d.txt" day)))
    | Part2Demo ->
        Printf.printf "Running part 2 against demo input for day %d:\n" day;
        Printf.printf "%s\n"
          (part2 (open_file (Printf.sprintf "inputs/day_%d_demo.txt" day)))
    | Part2 ->
        Printf.printf "Running part 2 against real input for day %d:\n" day;
        Printf.printf "%s\n"
          (part2 (open_file (Printf.sprintf "inputs/day_%d.txt" day)))
  in

  let usage_msg = Printf.sprintf "day%d [--help] [--action=ACTION]" day in
  let actions = ref [] in
  let speclist =
    [
      ( "--action",
        Arg.Symbol
          ( [ "tests"; "part1demo"; "part1"; "part2demo"; "part2" ],
            fun action -> actions := action_of_str action :: !actions ),
        "Set an action to be run (can be called multiple times)" );
    ]
  in

  Arg.parse speclist failwith usage_msg;
  List.iter do_action (List.rev !actions)

let lines (input : string) : string list =
  input |> String.split_on_char '\n' |> List.filter (fun s -> s <> "")

let print_day a = Printf.printf "Hello, today is day %d\n" a
