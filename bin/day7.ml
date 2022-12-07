open OUnit2

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

let string_of_filesystem (filesystem : filesystem) : string =
  let rec string_of_filesystem (depth : int) (filesystem : filesystem) : string
      =
    match filesystem with
    | FileEntry (name, size) ->
        String.make (depth * 2) ' '
        ^ Printf.sprintf "- %s (file, size=%d)" name size
    | DirectoryEntry (name, entries) ->
        String.make (depth * 2) ' '
        ^ Printf.sprintf "- %s (dir, size=%d)" name
            (List.hd (directory_sizes (DirectoryEntry (name, entries))))
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
    | [] -> (None, List.rev already_searched)
    | FileEntry _ :: _ ->
        failwith
          "file entry should never be present in the found directory list"
    | DirectoryEntry (name, contents) :: rest when name = needle_name ->
        ( Some (DirectoryEntry (name, contents)),
          List.rev (already_searched @ rest) )
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

let build_filesystem_tests =
  "test suite for build_filesystem"
  >::: [
         ( "builds a filesystem containing files in the root directory"
         >:: fun _ ->
           assert_equal ~printer:string_of_filesystem
             (DirectoryEntry
                ( "/",
                  [
                    FileEntry ("file1.txt", 100);
                    FileEntry ("file2.txt", 200);
                    FileEntry ("file3.txt", 300);
                  ] ))
             (build_filesystem
                [
                  Command (ChangeDirectory "/");
                  Command ListFiles;
                  ListFilesOutput (File ("file1.txt", 100));
                  ListFilesOutput (File ("file2.txt", 200));
                  ListFilesOutput (File ("file3.txt", 300));
                ]) );
         ( "builds a filesystem containing files in nested directories"
         >:: fun _ ->
           assert_equal ~printer:string_of_filesystem
             (DirectoryEntry
                ( "/",
                  [
                    DirectoryEntry
                      ( "second",
                        [
                          FileEntry ("file1.txt", 100);
                          FileEntry ("file2.txt", 200);
                          FileEntry ("file3.txt", 300);
                        ] );
                  ] ))
             (build_filesystem
                [
                  Command (ChangeDirectory "/");
                  Command ListFiles;
                  ListFilesOutput (Directory "second");
                  Command (ChangeDirectory "second");
                  Command ListFiles;
                  ListFilesOutput (File ("file1.txt", 100));
                  ListFilesOutput (File ("file2.txt", 200));
                  ListFilesOutput (File ("file3.txt", 300));
                ]) );
         ( "builds a filesystem containing consecutive directories" >:: fun _ ->
           assert_equal ~printer:string_of_filesystem
             (DirectoryEntry
                ( "/",
                  [
                    DirectoryEntry ("second", [ FileEntry ("file1.txt", 100) ]);
                    DirectoryEntry ("third", [ FileEntry ("file2.txt", 200) ]);
                    DirectoryEntry ("fourth", [ FileEntry ("file3.txt", 300) ]);
                  ] ))
             (build_filesystem
                [
                  Command (ChangeDirectory "/");
                  Command ListFiles;
                  ListFilesOutput (Directory "second");
                  ListFilesOutput (Directory "third");
                  ListFilesOutput (Directory "fourth");
                  Command (ChangeDirectory "second");
                  Command ListFiles;
                  ListFilesOutput (File ("file1.txt", 100));
                  Command ChangeUpDirectory;
                  Command (ChangeDirectory "third");
                  Command ListFiles;
                  ListFilesOutput (File ("file2.txt", 200));
                  Command ChangeUpDirectory;
                  Command (ChangeDirectory "fourth");
                  Command ListFiles;
                  ListFilesOutput (File ("file3.txt", 300));
                ]) );
       ]

let part_1 (input : string) : string =
  let filesystem = input |> Aoclib.lines |> parse_lines |> build_filesystem in
  filesystem |> directory_sizes
  |> List.filter (( > ) 100000)
  |> List.fold_left ( + ) 0 |> string_of_int

let part_2 (input : string) : string =
  let filesystem = input |> Aoclib.lines |> parse_lines |> build_filesystem in
  let to_be_deleted =
    30000000 - (70000000 - List.hd (directory_sizes filesystem))
  in
  Printf.printf "total size of directory = %d\n"
    (List.hd (directory_sizes filesystem));
  Printf.printf "total free space        = %d\n"
    (70000000 - List.hd (directory_sizes filesystem));
  Printf.printf "free space necessary    = %d\n" 30000000;
  Printf.printf "space to be freed:      = %8d\n"
    (30000000 - (70000000 - List.hd (directory_sizes filesystem)));
  (* print_endline (string_of_filesystem filesystem); *)
  print_int to_be_deleted;
  print_newline ();
  filesystem |> directory_sizes
  (* |> List.filter (fun size -> size > to_be_deleted) *)
  |> List.fold_left
       (fun acc size ->
         Printf.printf "deciding to delete ? %d (> %d, ~ %d)\n" size
           to_be_deleted acc;
         if size > to_be_deleted && size < acc then size else acc)
       70000000
  |> string_of_int

let () =
  Aoclib.aoc ~tests:[ build_filesystem_tests ] ~part1:part_1 ~part2:part_2 7
