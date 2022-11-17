open Printf
open Effect
open Effect.Deep

module type Scheduler = sig
  val async : (unit -> 'a) -> unit
  (** [async f] runs [f] concurrently *)
  val yield : unit -> unit
  (** yields control to another task *)
  val run   : (unit -> 'a) -> unit
  (** Runs the scheduler *)
end

module Scheduler : Scheduler = struct

  type _ Effect.t += Async : (unit -> 'a) -> unit Effect.t
                   | Yield : unit Effect.t

  let async f = perform (Async f)

  let yield () = perform Yield

  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () =
    if Queue.is_empty q then ()
    else Queue.pop q ()

  let rec run : 'a. (unit -> 'a) -> unit =
    fun main ->
      match_with main ()
      { retc = (fun _ -> dequeue ());
        exnc = (fun e -> raise e);
        effc = (fun (type b) (eff: b Effect.t) ->
            match eff with
            | Async f -> Some (fun (k: (b, _) continuation) ->
                    enqueue (continue k);
                    run f
            )
            | Yield -> Some (fun k ->
                    enqueue (continue k);
                    dequeue ()
            )
            | _ -> None
        )}
end

open Scheduler

let main () =
  let mk_task name () =
    printf "starting %s\n%!" name;
    yield ();
    printf "ending %s\n%!" name
  in
  async (mk_task "a");
  async (mk_task "b")

let _ = run main
