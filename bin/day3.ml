open OUnit2

type compartments = string * string

let split_string_half (input : string) : compartments =
  let length = String.length input in
  ( String.sub input 0 (length / 2),
    String.sub input (length / 2) (length - (length / 2)) )

let split_string_half_tests =
  "test suite for split_string_half"
  >::: [
         ( "splits empty list correctly" >:: fun _ ->
           assert_equal ("", "") (split_string_half "") );
         ( "splits list of two elements evenly" >:: fun _ ->
           assert_equal ("a", "b") (split_string_half "ab") );
         ( "splits list of three elements approximately evenly" >:: fun _ ->
           assert_equal ("a", "bc") (split_string_half "abc") );
       ]

module CS = Set.Make (Char)

let shared_element ((left_str, right_str) : compartments) : char =
  let left = String.fold_right CS.add left_str CS.empty in
  let maybe_found =
    String.fold_right
      (fun c acc -> if CS.mem c left then Some c else acc)
      right_str None
  in
  Option.get maybe_found

let shared_element_tests =
  "test suite for shared_element"
  >::: [
         ( "finds the shared char in two strings" >:: fun _ ->
           assert_equal ~printer:Char.escaped 'c' (shared_element ("c", "c")) );
         ( "finds the shared char in two long strings" >:: fun _ ->
           assert_equal ~printer:Char.escaped 'D'
             (shared_element ("AaBbCcDd", "DEeFfGgHh")) );
       ]

let priority (c : char) : int =
  let ascii = Char.code c in
  if ascii > 96 then ascii - 96
  else if ascii > 64 then ascii - 64 + 26
  else failwith "invalid character"

let priority_tests =
  "test suite for priority"
  >::: [
         ( "gets the priority for lowercase a" >:: fun _ ->
           assert_equal ~printer:string_of_int 1 (priority 'a') );
         ( "gets the priority for uppercase A" >:: fun _ ->
           assert_equal ~printer:string_of_int 27 (priority 'A') );
         ( "gets the priority for lowercase z" >:: fun _ ->
           assert_equal ~printer:string_of_int 26 (priority 'z') );
         ( "gets the priority for uppercase Z" >:: fun _ ->
           assert_equal ~printer:string_of_int 52 (priority 'Z') );
       ]

let group3 (items : 'a list) : ('a * 'a * 'a) list =
  let rec grouping items current_set groups =
    match (items, current_set) with
    | [], [] -> []
    | [], [ a; b; c ] -> List.rev ((a, b, c) :: groups)
    | head :: tail, [ a; b; c ] -> grouping tail [ head ] ((a, b, c) :: groups)
    | head :: tail, [ a; b ] -> grouping tail [ a; b; head ] groups
    | head :: tail, [ a ] -> grouping tail [ a; head ] groups
    | head :: tail, [] -> grouping tail [ head ] groups
    | _ -> failwith "items is not divisible by three"
  in
  grouping items [] []

let group3_tests =
  "test suite for group3"
  >::: [
         ("an empty list remains empty" >:: fun _ -> assert_equal [] (group3 []));
         ( "a list of three elements gets split once" >:: fun _ ->
           assert_equal [ (1, 2, 3) ] (group3 [ 1; 2; 3 ]) );
         ( "a list of six elements gets split twice" >:: fun _ ->
           assert_equal [ (1, 2, 3); (4, 5, 6) ] (group3 [ 1; 2; 3; 4; 5; 6 ])
         );
       ]

let shared3 ((left, middle, right) : string * string * string) : char =
  let left = String.fold_right CS.add left CS.empty in
  let middle = String.fold_right CS.add middle CS.empty in
  let maybe_found =
    String.fold_right
      (fun c acc -> if CS.mem c left && CS.mem c middle then Some c else acc)
      right None
  in
  Option.get maybe_found

let input (input_file : string) : string list =
  let contents = In_channel.with_open_bin input_file In_channel.input_all in
  contents |> String.split_on_char '\n' |> List.filter (fun s -> s <> "")

let () =
  run_test_tt_main
    ("All tests"
    >::: [
           split_string_half_tests;
           shared_element_tests;
           priority_tests;
           group3_tests;
         ]);
  print_endline "test data part 1";
  print_int
    ("inputs/day_3_demo.txt" |> input |> List.map split_string_half
   |> List.map shared_element |> List.map priority |> List.fold_left ( + ) 0);
  print_newline ();
  print_endline "part 1";
  print_int
    ("inputs/day_3.txt" |> input |> List.map split_string_half
   |> List.map shared_element |> List.map priority |> List.fold_left ( + ) 0);
  print_newline ();
  print_endline "test data part 2";
  print_int
    ("inputs/day_3_demo.txt" |> input |> group3 |> List.map shared3
   |> List.map priority |> List.fold_left ( + ) 0);
  print_newline ();
  print_endline "part 1";
  print_int
    ("inputs/day_3.txt" |> input |> group3 |> List.map shared3
   |> List.map priority |> List.fold_left ( + ) 0);
  print_newline ()
