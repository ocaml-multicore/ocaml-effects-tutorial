type bottom (* uninhabited type *)
(** bottom type has no inhabitants. No value has this type. *)

type exn = bottom eff
(** An exception is an effect that never returns *)

let raise (e : exn) : 'a = failwith "not implemented"
(* TODO *)

let try_with (f : unit -> 'a) (h : exn -> 'a) : 'a = failwith "not implemented"
(* TODO *)

effect Invalid_argument : bottom
(* Invalid argument is an exception *)

let _ : exn = Invalid_argument
(* And it has type [exn] *)

(** [sqrt f] returns the square root of [f].
    @raise Invalid_argument if f < 0. *)
let sqrt f =
  if f < 0.0 then raise Invalid_argument
  else sqrt f

let _ =
  try_with (fun () ->
    let r = sqrt 42.42 in
    Printf.printf "%f\n%!" r;
    let r = sqrt (-1.0) in
    Printf.printf "%f\n" r)
  (fun Invalid_argument -> Printf.printf "Invalid_argument to sqrt\n")

(* Prints:
   6.513064
   Invalid_argument to sqrt *)
