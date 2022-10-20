open Printf
open Effect
open Effect.Shallow

module type STATE = sig
  type t
  val put     : t -> unit
  val get     : unit -> t
  val history : unit -> t list
  val run : (unit -> unit) -> init:t -> unit
end

module State (S : sig type t end) : STATE with type t = S.t = struct

  type t = S.t

  type _ Effect.t += Get : t Effect.t

  let get () = perform Get

  let put v = failwith "not implemented"

  let history () = failwith "not implemented"

  let run f ~init =

    let rec loop : type a r. t -> (a, r) continuation -> a -> r =
      fun state k x ->
        continue_with k x
        { retc = (fun result -> result);
          exnc = (fun e -> raise e);
          effc = (fun (type b) (eff: b Effect.t) ->
            match eff with
            | Get -> Some (fun (k: (b,r) continuation) ->
                    failwith "not implemented")
            | _ -> None)
        }
    in
    loop init (fiber f) ()
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

let _ = IS.run (fun () -> SS.run foo ~init:"") ~init:0
