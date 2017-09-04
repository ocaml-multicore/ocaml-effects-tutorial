open Printf

module type STATE = sig
  type t
  val get : unit -> t
  val run : (unit -> unit) -> init:t -> unit
end

module State (S : sig type t end) : STATE with type t = S.t = struct

  type t = S.t

  effect Get : t
  let get () = perform Get

  let run f ~init =
    let comp =
      match f () with
      | () -> (fun s -> ())
      | effect Get k -> (fun (s : t) -> continue k s s)
    in comp init
end

module IS = State (struct type t = int end)
module SS = State (struct type t = string end)

let foo () : unit =
  printf "%d\n" (IS.get ());
  printf "%d\n" (IS.get ());
  printf "%d\n" (IS.get ());
  printf "%s\n" (SS.get ());
  printf "%s\n" (SS.get ())

let _ = IS.run (fun () -> SS.run foo "forty two") 42
