[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-26"]

open Lwt.Infix
open Cohttp_lwt_unix

type todo =
  { user_id : int
  ; id : int
  ; title : string
  ; completed : bool
  }

let parse_string raw_string =
  try
    Ok
      (let qwe = Yojson.Safe.from_string raw_string in
       qwe)
  with
  | Yojson.Json_error e -> Error e
;;

let decode_todo json =
  let open Yojson.Safe.Util in
  try
    Ok
      { user_id = json |> member "userId" |> to_int
      ; id = json |> member "id" |> to_int
      ; title = json |> member "title" |> to_string
      ; completed = json |> member "completed" |> to_bool
      }
  with
  (* | Yojson.Json_error e -> Error e *)
  | Yojson.Safe.Util.Type_error (msg, value) -> Error (msg, value)
;;

(** open Cohttp
    let (>>) f g x = g (f x);;
    let (<<) g f x = g (f x);;

    let myText = "\nHello, World " ;;
    **)

let get (url : string) : string Lwt.t =
  Client.get (Uri.of_string url)
  >>= fun (_resp, body) -> Cohttp_lwt.Body.to_string body
;;

let prefix = "\nbegin prog\n"
let suffix = "\nend prog\n"
let counts = [ 1; 2; 3; 4 ]
let _test () = "hello!"

let main () =
  Printf.printf "\nBEGIN %s\n" "";
  let body : string =
    Lwt_main.run (get "https://jsonplaceholder.typicode.com/todos/1")
  in
  let parse_result = parse_string {|{"title":"josh"}|} (* body *) in
  Printf.printf "\nREAD %s\n" "";
  let _ =
    match parse_result with
    | Ok valid_parse_result ->
      let decoded_todo_result : (todo, string * Yojson.Safe.t) result =
        decode_todo valid_parse_result
      in
      (match decoded_todo_result with
       | Ok valid_todo ->
         Printf.printf "\nDECODE %s\n" "";
         let stringedInts = List.map string_of_int counts in
         let oneWord = String.concat ", " stringedInts in
         print_endline (prefix ^ oneWord ^ suffix);
         (* Printf.printf "\n%s!!\n" body *)
         Printf.printf "\n%s!!\n" valid_todo.title
       | Error (err_string, value) ->
         print_endline "ERROR";
         print_endline err_string)
    | Error err_string ->
      print_endline "ERROR";
      print_endline err_string
  in
  print_endline "END"
;;

let () = main ()
