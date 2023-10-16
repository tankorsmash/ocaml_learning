[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-32"]

open Lwt.Infix
open Cohttp_lwt_unix;;

Printf.printf "\n"

let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)
let fst (x, _) = x
let snd (_, x) = x

type todo =
  { user_id : int
  ; id : int
  ; title : string
  ; completed : bool
  }

let parse_string raw_string =
  try Ok (Yojson.Safe.from_string raw_string) with
  | Yojson.Json_error e -> Error e
;;

let my_rng () =
  let rng = Random.State.make [| 1; 2; 3 |] in
  Random.State.int rng 100, rng
;;

let rand_int rng max =
  let n = Random.State.int rng max in
  n, rng
;;

let num = my_rng >> snd >> (fun rng -> rand_int rng 100) >> fst;;

Printf.printf "NUMBER %d\n" @@ fst @@ my_rng ();;
Printf.printf "NUMBER %d\n" @@ num ();;
Printf.printf "NUMBER %d\n" @@ fst @@ my_rng ();;
Printf.printf "NUMBER %d\n" @@ fst @@ my_rng ()

let decode_todo json =
  let open Yojson.Safe.Util in
  let json_member name = json |> member name in
  try
    Ok
      { user_id = json_member "userId" |> to_int
      ; id = json_member "id" |> to_int
      ; title = json_member "title" |> to_string
      ; completed = json_member "completed" |> to_bool
      }
  with
  | Yojson.Safe.Util.Type_error (msg, value) -> Error (msg, value)
;;

let get (url : string) : string Lwt.t =
  Client.get (Uri.of_string url)
  >>= fun (_resp, body) -> Cohttp_lwt.Body.to_string body
;;

let prefix = "\nbegin prog\n"
let suffix = "\nend prog\n"
let counts = [ 1; 2; 3; 4 ]
let _test () = "hello!"
let global_var = ref 1

let main () =
  global_var := !global_var + 1;
  Printf.printf "\nBEGIN %d\n" !global_var;
  let body : string =
    Lwt_main.run (get "https://jsonplaceholder.typicode.com/todos/1")
  in
  Printf.printf "\nAGAIN %d\n" !global_var;
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
