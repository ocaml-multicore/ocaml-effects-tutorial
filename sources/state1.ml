open Printf
open Effect
open Effect.Shallow

module type STATE = sig
  type t
  val get : unit -> t
  val run : (unit -> unit) -> init:t -> unit
end

module State (S : sig type t end) : STATE with type t = S.t = struct

  type t = S.t

  type _ Effect.t += Get : t Effect.t

  let get () = perform Get

  let run f ~init =
    let rec loop : type a r. t -> (a, r) continuation -> a -> r =
      fun state k x ->
        continue_with k x
        { retc = (fun result -> result);
          exnc = (fun e -> raise e);
          effc = (fun (type b) (eff: b Effect.t) ->
            match eff with
            | Get -> Some (fun (k: (b,r) continuation) ->
                    loop state k state)
            | _ -> None)
        }
    in
    loop init (fiber f) ()
end

module IS = State (struct type t = int end)
module SS = State (struct type t = string end)

let foo () : unit =
  printf "%d\n" (IS.get ());
  printf "%d\n" (IS.get ());
  printf "%d\n" (IS.get ());
  printf "%s\n" (SS.get ());
  printf "%s\n" (SS.get ())

let _ = IS.run (fun () -> SS.run foo ~init:"forty two") ~init:42
