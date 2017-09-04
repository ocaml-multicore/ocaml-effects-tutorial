exception Conversion_failure of string

let int_of_string l =
  try int_of_string l with
  | Failure _ -> raise (Conversion_failure l)

let rec sum_up acc =
    let l = input_line stdin in
    acc := !acc + int_of_string l;
    sum_up acc

let _ =
  let r = ref 0 in
  try sum_up r with
  | End_of_file -> Printf.printf "Sum is %d\n" !r
  | Conversion_failure s ->
      Printf.fprintf stderr "Conversion failure \"%s\"\n%!" s

