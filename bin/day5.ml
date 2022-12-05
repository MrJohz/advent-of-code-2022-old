open OUnit2

type stack = char list
type stacks = stack list
type command = { count : int; from_col : int; to_col : int }

let string_of_command ({ count; from_col; to_col } : command) =
  Printf.sprintf "{ count = %d; from_col = %d; to_col = %d }" count from_col
    to_col

let parse_single_row (input : string) : stacks =
  let rec parse_single_row (input : string) (pos : int) (row : stacks) : stacks
      =
    if input == "" then List.rev row
    else
      let length = String.length input in
      let char = input.[1] in
      let rest =
        if length == 3 then "" else String.sub input 4 (String.length input - 4)
      in
      if char == ' ' then parse_single_row rest (pos + 1) ([] :: row)
      else parse_single_row rest (pos + 1) ([ char ] :: row)
  in
  parse_single_row input 0 []

let parse_single_row_tests =
  "test suite for parse_single_row"
  >::: [
         ( "parses an empty row by returning an empty list of columns"
         >:: fun _ -> assert_equal [] (parse_single_row "") );
         ( "parses a row containing a single column by returning that column"
         >:: fun _ -> assert_equal [ [ 'A' ] ] (parse_single_row "[A]") );
         ( "parses a row containing two columns by returning those columns"
         >:: fun _ ->
           assert_equal [ [ 'A' ]; [ 'B' ] ] (parse_single_row "[A] [B]") );
         ( "parses a row with empty columns by returning nothing" >:: fun _ ->
           assert_equal [ []; [] ] (parse_single_row "       ") );
         ( "parses a mix of empty cols and boxes" >:: fun _ ->
           assert_equal [ []; [ 'C' ] ] (parse_single_row "    [C]") );
       ]

let combine_stacks (stacks : stacks list) : stacks =
  let rec combine_stacks (stack_inputs : stacks list) (stacks : stacks) : stacks
      =
    match stack_inputs with
    | [] -> stacks
    | last_stacks :: stack_inputs ->
        combine_stacks stack_inputs
          (List.combine stacks last_stacks
          |> List.map (fun (prev, next) -> List.concat [ next; prev ]))
  in
  combine_stacks (List.tl stacks) (List.hd stacks)

let combine_stacks_tests =
  "test suite for combine_stacks"
  >::: [
         ( "combines a single set of stacks with itself" >:: fun _ ->
           assert_equal [ [ 'A' ]; [ 'B' ] ]
             (combine_stacks [ [ [ 'A' ]; [ 'B' ] ] ]) );
         ( "combines two stacks with themselves" >:: fun _ ->
           assert_equal
             [ [ 'A'; 'C' ]; [ 'B'; 'D' ] ]
             (combine_stacks [ [ [ 'C' ]; [ 'D' ] ]; [ [ 'A' ]; [ 'B' ] ] ]) );
         ( "handles empty slots in stacks" >:: fun _ ->
           assert_equal
             [ [ 'A'; 'C'; 'E' ]; [ 'B'; 'D' ]; [ 'F' ] ]
             (combine_stacks
                [
                  [ [ 'E' ]; []; [] ];
                  [ [ 'C' ]; [ 'D' ]; [] ];
                  [ [ 'A' ]; [ 'B' ]; [ 'F' ] ];
                ]) );
       ]

let parse_stacks (input : string list) : stacks * string list =
  let rec parse_stacks (input : string list) (stacks : stacks list) :
      stacks * string list =
    match input with
    | [] ->
        failwith "input should not end before the stacks have finished parsing"
    | head :: body when String.starts_with ~prefix:" 1 " head ->
        (combine_stacks stacks, body)
    | head :: body -> parse_stacks body (parse_single_row head :: stacks)
  in
  parse_stacks input []

let parse_command (input : string) : command =
  let re =
    Re.compile
      Re.(
        seq
          [
            str "move ";
            group (rep1 digit);
            str " from ";
            group (rep1 digit);
            str " to ";
            group (rep1 digit);
          ])
  in
  let groups = Re.exec re input in
  {
    count = int_of_string (Re.Group.get groups 1);
    from_col = int_of_string (Re.Group.get groups 2);
    to_col = int_of_string (Re.Group.get groups 3);
  }

let parse_command_tests =
  "test suite for parse_command"
  >::: [
         ( "parses a command with single digits" >:: fun _ ->
           assert_equal ~printer:string_of_command
             { count = 1; from_col = 2; to_col = 3 }
             (parse_command "move 1 from 2 to 3") );
         ( "parses a command with single digits" >:: fun _ ->
           assert_equal ~printer:string_of_command
             { count = 12; from_col = 23; to_col = 34 }
             (parse_command "move 12 from 23 to 34") );
       ]

let parse (input : string list) : stacks * command list =
  let stacks, commands = parse_stacks input in
  let commands = List.map parse_command commands in
  (stacks, commands)

let () =
  Aoclib.aoc
    ~tests:[ parse_single_row_tests; combine_stacks_tests; parse_command_tests ]
    4
