type command = ListFiles | ChangeUpDirectory | ChangeDirectory of string
type ls_output = Directory of string | File of string * int
type line = Command of command | ListFilesOutput of ls_output

let reg_list_files = Re.compile Re.(str "$ ls")
let reg_change_up_directory = Re.compile Re.(str "$ cd ..")
let reg_change_directory = Re.compile Re.(seq [ str "$ cd "; group (rep1 any) ])
let reg_directory_output = Re.compile Re.(seq [ str "dir "; group (rep1 any) ])

let reg_file_output =
  Re.compile Re.(seq [ group (rep1 digit); str " "; group (rep1 any) ])

let parse_lines (lines : string list) : line list =
  List.map
    (fun line ->
      match Re.exec_opt reg_list_files line with
      | Some _ -> Command ListFiles
      | None -> (
          match Re.exec_opt reg_change_up_directory line with
          | Some _ -> Command ChangeUpDirectory
          | None -> (
              match Re.exec_opt reg_change_directory line with
              | Some group -> Command (ChangeDirectory (Re.Group.get group 1))
              | None -> (
                  match Re.exec_opt reg_directory_output line with
                  | Some group ->
                      ListFilesOutput (Directory (Re.Group.get group 1))
                  | None -> (
                      match Re.exec_opt reg_file_output line with
                      | Some group ->
                          ListFilesOutput
                            (File
                               ( Re.Group.get group 2,
                                 int_of_string (Re.Group.get group 1) ))
                      | None -> failwith "line could not be parsed")))))
    lines

type filesystem =
  | FileEntry of string * int
  | DirectoryEntry of string * filesystem list

let string_of_filesystem (filesystem : filesystem) : string =
  let rec string_of_filesystem (depth : int) (filesystem : filesystem) : string
      =
    match filesystem with
    | FileEntry (name, size) ->
        String.make (depth * 2) ' '
        ^ Printf.sprintf "- %s (file, size=%d)" name size
    | DirectoryEntry (name, entries) ->
        String.make (depth * 2) ' '
        ^ Printf.sprintf "- %s (dir)" name
        ^ String.concat ""
            (List.map
               (fun each -> "\n" ^ string_of_filesystem (depth + 1) each)
               entries)
  in
  string_of_filesystem 0 filesystem

let build_filesystem (inputs : line list) : filesystem =
  let rec find_found_directory (needle_name : string)
      (found_dirs : filesystem list) (already_searched : filesystem list) :
      filesystem option * filesystem list =
    match found_dirs with
    | [] -> (None, already_searched @ found_dirs)
    | FileEntry _ :: _ ->
        failwith
          "file entry should never be present in the found directory list"
    | DirectoryEntry (name, contents) :: rest when name == needle_name ->
        (Some (DirectoryEntry (name, contents)), already_searched @ rest)
    | DirectoryEntry (name, contents) :: rest ->
        find_found_directory needle_name rest
          (DirectoryEntry (name, contents) :: already_searched)
  in
  let rec build_filesystem (inputs : line list) (found_dirs : filesystem list)
      (files : filesystem list) : filesystem list =
    match inputs with
    | [] -> files
    | ListFilesOutput (File (name, size)) :: rest ->
        build_filesystem rest found_dirs (FileEntry (name, size) :: files)
    | ListFilesOutput (Directory name) :: rest -> (
        match find_found_directory name found_dirs [] with
        | Some dir, found_dirs -> build_filesystem rest found_dirs (dir :: files)
        | None, found_dirs -> build_filesystem rest found_dirs files)
    | Command ListFiles :: rest -> build_filesystem rest found_dirs files
    | Command ChangeUpDirectory :: rest ->
        build_filesystem rest (found_dirs @ files) []
    | Command (ChangeDirectory dir) :: rest ->
        build_filesystem rest found_dirs [ DirectoryEntry (dir, files) ]
  in
  match build_filesystem (List.rev inputs) [] [] with
  | [ root ] -> root
  | _ -> failwith "a correct input will have exactly one root element"

let directory_sizes (filesystem : filesystem) : int list =
  let rec part1 (filesystem : filesystem) : int list * int =
    match filesystem with
    | FileEntry (_, size) -> ([], size)
    | DirectoryEntry (_, children) ->
        let child_sizes, sum =
          List.fold_left
            (fun (prev_sizes, prev_size) child ->
              let dirs, size = part1 child in
              (prev_sizes @ dirs, prev_size + size))
            ([], 0) children
        in
        (sum :: child_sizes, sum)
  in
  let dirs, current_size = part1 filesystem in
  current_size :: dirs

let part_1 (input : string) : string =
  let filesystem = input |> Aoclib.lines |> parse_lines |> build_filesystem in
  print_endline (string_of_filesystem filesystem);
  filesystem |> directory_sizes
  |> List.filter (( > ) 100000)
  |> List.fold_left ( + ) 0 |> string_of_int

let () = Aoclib.aoc ~part1:part_1 7
