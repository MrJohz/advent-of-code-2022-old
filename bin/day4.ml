open OUnit2

type range = int * int

let parse (input : string) : range * range =
  let parse_range (input : string) =
    match String.split_on_char '-' input with
    | [ left; right ] -> (int_of_string left, int_of_string right)
    | _ -> failwith "ranges must be two numbers separated by a dash"
  in

  match String.split_on_char ',' input with
  | [ left; right ] -> (parse_range left, parse_range right)
  | _ -> failwith "input must contain a single comma"

let parse_tests =
  "test suite for parse"
  >::: [
         ( "parses a range successfully" >:: fun _ ->
           assert_equal ((1, 2), (3, 4)) (parse "1-2,3-4") );
       ]

let contains ((a_start, a_end) : range) ((b_start, b_end) : range) : bool =
  a_start <= b_start && a_end >= b_end

let contains_tests =
  "test suite for contains"
  >::: [
         ( "true when a range is entirely within another range" >:: fun _ ->
           assert_equal ~printer:string_of_bool true (contains (1, 5) (2, 4)) );
         ( "true when ranges have one equal boundary" >:: fun _ ->
           assert_equal ~printer:string_of_bool true (contains (1, 5) (1, 4)) );
         ( "true when ranges are identical" >:: fun _ ->
           assert_equal ~printer:string_of_bool true (contains (1, 5) (1, 5)) );
         ( "false when ranges overlap" >:: fun _ ->
           assert_equal ~printer:string_of_bool false (contains (1, 4) (3, 7))
         );
         ( "false when other range contains first range" >:: fun _ ->
           assert_equal ~printer:string_of_bool false (contains (2, 4) (1, 5))
         );
       ]

let rec overlap ((a_start, a_end) : range) ((b_start, b_end) : range) : bool =
  if a_start <= b_start then a_start <= b_end && a_end >= b_start
  else overlap (b_start, b_end) (a_start, a_end)

let overlap_tests =
  "test suite for overlap"
  >::: [
         ( "false when ranges are completely different" >:: fun _ ->
           assert_equal ~printer:string_of_bool false (overlap (2, 4) (6, 8)) );
         ( "false when ranges are different but inverted" >:: fun _ ->
           assert_equal ~printer:string_of_bool false (overlap (4, 5) (2, 3)) );
         ( "true when ranges overlap by one number" >:: fun _ ->
           assert_equal ~printer:string_of_bool true (overlap (5, 7) (7, 9)) );
         ( "true when one range has only one element and the ranges overlap"
         >:: fun _ ->
           assert_equal ~printer:string_of_bool true (overlap (6, 6) (6, 9)) );
         ( "true when one range has only one element and the ranges overlap \
            inverted"
         >:: fun _ ->
           assert_equal ~printer:string_of_bool true (overlap (6, 9) (6, 6)) );
         ( "true when one range has only one element and it lies within the \
            other"
         >:: fun _ ->
           assert_equal ~printer:string_of_bool true (overlap (6, 6) (4, 9)) );
         ( "true when the second range contains the first" >:: fun _ ->
           assert_equal ~printer:string_of_bool true (overlap (4, 9) (6, 6)) );
         ( "true when ranges are identical" >:: fun _ ->
           assert_equal ~printer:string_of_bool true (overlap (1, 5) (1, 5)) );
         ( "true when ranges overlap" >:: fun _ ->
           assert_equal ~printer:string_of_bool true (overlap (1, 4) (3, 7)) );
         ( "true when ranges overlap inverted" >:: fun _ ->
           assert_equal ~printer:string_of_bool true (overlap (3, 7) (1, 4)) );
         ( "true when other range contains first range" >:: fun _ ->
           assert_equal ~printer:string_of_bool true (overlap (2, 4) (1, 5)) );
         ( "false when ranges have no overlap" >:: fun _ ->
           assert_equal ~printer:string_of_bool false (overlap (2, 3) (6, 8)) );
       ]

let () =
  Aoclib.aoc
    ~tests:[ parse_tests; contains_tests; overlap_tests ]
    ~part1:(fun input ->
      input |> Aoclib.lines |> List.map parse
      |> List.fold_left
           (fun acc (left, right) ->
             if contains left right || contains right left then acc + 1 else acc)
           0
      |> string_of_int)
    ~part2:(fun input ->
      input |> Aoclib.lines |> List.map parse
      |> List.fold_left
           (fun acc (left, right) ->
             if overlap left right then acc + 1 else acc)
           0
      |> string_of_int)
    4
