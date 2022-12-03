let group (input : 'a option list) : 'a list list =
  let rec grouping all_lists current_list = function
    | [] -> List.rev (List.rev current_list) :: all_lists
    | Some a :: tl -> grouping all_lists (a :: current_list) tl
    | None :: tl -> grouping (List.rev current_list :: all_lists) [] tl
  in
  grouping [] [] input

let sum_groups (input : int list list) = List.map (List.fold_left ( + ) 0) input

let largest (input : int list) =
  List.fold_left (fun acc elem -> if elem > acc then elem else acc) 0 input

let largest_three (input : int list) =
  List.fold_left
    (fun (acc1, acc2, acc3) elem ->
      if elem > acc1 then (elem, acc1, acc2)
      else if elem > acc2 then (acc1, elem, acc2)
      else if elem > acc3 then (acc1, acc2, elem)
      else (acc1, acc2, acc3))
    (0, 0, 0) input

let part_1 (input : string) : int =
  input |> String.split_on_char '\n' |> List.map int_of_string_opt |> group
  |> sum_groups |> largest

let part_2 (input : string) : int =
  let n1, n2, n3 =
    input |> String.split_on_char '\n' |> List.map int_of_string_opt |> group
    |> sum_groups |> largest_three
  in
  n1 + n2 + n3

let () = Aoclib.aoc ~part1:part_1 ~part2:part_2 1
