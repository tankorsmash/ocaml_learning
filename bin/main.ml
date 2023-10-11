(**
let (>>) f g x = g (f x);;
let (<<) g f x = g (f x);;


let myText = "\nHello, World " ;;
**)
let prefix = "\nbegin prog\n" ;;
let suffix = "\nend prog\n" ;;

let counts = [1; 2; 3; 4] ;;
let () = 
        let stringedInts = List.map (string_of_int) counts in
        let oneWord = String.concat ", " stringedInts in
        print_endline (prefix ^ oneWord ^ suffix)


