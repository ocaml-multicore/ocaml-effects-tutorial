effect Exn : exn -> 'a

let raise (e : exn) : 'a = perform (Exn e)

let try_with (f : unit -> 'a) (h : exn -> 'a) : 'a =
  try f () with
  | effect (Exn e) k -> h e

exception Invalid_argument

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
