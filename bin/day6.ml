type marker = { length : int; contents : char list }

let empty_marker (size : int) = { length = size; contents = [] }
let is_partial (marker : marker) = marker.length != List.length marker.contents

let update (new_elem : char) (marker : marker) : marker =
  let rec except_last l acc =
    match l with
    | [] | [ _ ] -> List.rev acc
    | head :: tail -> except_last tail (head :: acc)
  in
  if is_partial marker then
    { marker with contents = new_elem :: marker.contents }
  else { marker with contents = new_elem :: except_last marker.contents [] }

let parse_buffer (prev_buffer : marker) (stream : char list) :
    marker * char list =
  match stream with
  | next :: tail -> (update next prev_buffer, tail)
  | [] -> failwith "the stream ran out before a valid marker could be found"

module SC = Set.Make (Char)

let buffer_is_marker (buffer : marker) =
  (not (is_partial buffer))
  && SC.cardinal (List.fold_right SC.add buffer.contents SC.empty)
     == List.length buffer.contents

let find_marker (marker : marker) (stream : char list) : int * marker =
  let rec find_marker (buffer : marker) (stream : char list) (index : int) =
    if buffer_is_marker buffer then (index, buffer)
    else
      let buffer, stream = parse_buffer buffer stream in
      find_marker buffer stream (index + 1)
  in
  find_marker marker stream 0

let explode s = List.init (String.length s) (String.get s)

let () =
  Aoclib.aoc
    ~part1:(fun stream ->
      let pos, _ = stream |> explode |> find_marker (empty_marker 4) in
      string_of_int pos)
    ~part2:(fun stream ->
      let pos, _ = stream |> explode |> find_marker (empty_marker 14) in
      string_of_int pos)
    6
