open Printf

module type STATE = sig
  type t
  val put     : t -> unit
  val get     : unit -> t
  val history : unit -> t list
  val run : (unit -> unit) -> init:t -> unit
end

module State (S : sig type t end) : STATE with type t = S.t = struct

  type t = S.t

  effect Get : t
  let get () = perform Get

  effect Put : t -> unit
  let put v = perform (Put v)

  effect History : t list
  let history () = perform History

  let run f ~init =
    let comp : (t * t list) -> unit =
      match f () with
      | () -> (fun _ -> ())
      | effect Get k -> (fun (s,h) -> continue k s (s,h))
      | effect (Put s) k -> (fun (_,h) -> continue k () (s,s::h))
      | effect History k -> (fun (s,h) -> continue k (List.rev h) (s,h))
    in comp (init, [])
end

module IS = State (struct type t = int end)
module SS = State (struct type t = string end)

let foo () : unit =
  assert (0 = IS.get ());
  IS.put 42;
  assert (42 = IS.get ());
  IS.put 21;
  assert (21 = IS.get ());
  SS.put "hello";
  assert ("hello" = SS.get ());
  SS.put "world";
  assert ("world" = SS.get ());
  assert ([42; 21] = IS.history ())

let _ = IS.run (fun () -> SS.run foo "") 0
