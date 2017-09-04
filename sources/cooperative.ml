open Printf

module type Scheduler = sig
  val async : (unit -> 'a) -> unit
  (** [async f] runs [f] concurrently *)
  val yield : unit -> unit
  (** yields control to another task *)
  val run   : (unit -> 'a) -> unit
  (** Runs the scheduler *)
end

module Scheduler : Scheduler = struct

  effect Async : (unit -> 'a) -> unit
  let async f = perform (Async f)

  effect Yield : unit
  let yield () = perform Yield

  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () =
    if Queue.is_empty q then ()
    else Queue.pop q ()

  let rec run : 'a. (unit -> 'a) -> unit =
    fun main ->
      match main () with
      | _ -> dequeue ()
      | effect (Async f) k ->
          enqueue (continue k);
          run f
      | effect Yield k ->
          enqueue (continue k);
          dequeue ()
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
