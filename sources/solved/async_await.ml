open Printf

module type Scheduler = sig
  type 'a promise
  (** Type of promises *)
  val async : (unit -> 'a) -> 'a promise
  (** [async f] runs [f] concurrently *)
  val await : 'a promise -> 'a
  (** [await p] returns the result of the promise. *)
  val yield : unit -> unit
  (** yields control to another task *)
  val run   : (unit -> 'a) -> unit
  (** Runs the scheduler *)
end

module Scheduler : Scheduler = struct

  type 'a _promise =
    Waiting of ('a,unit) continuation list
  | Done of 'a

  type 'a promise = 'a _promise ref

  effect Async : (unit -> 'a) -> 'a promise
  let async f = perform (Async f)

  effect Yield : unit
  let yield () = perform Yield

  effect Await : 'a promise -> 'a
  let await p = perform (Await p)

  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () =
    if Queue.is_empty q then ()
    else Queue.pop q ()

  let run main =
    let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit =
      fun pr main ->
        match main () with
        | v ->
            let l = match !pr with Waiting l -> l | _ -> failwith "impossible" in
            List.iter (fun k -> enqueue (fun () -> continue k v)) l;
            pr := Done v;
            dequeue ()
        | effect (Async f) k ->
            let pr = ref (Waiting []) in
            enqueue (fun () -> continue k pr);
            fork pr f
        | effect Yield k ->
            enqueue (continue k);
            dequeue ()
        | effect (Await p) k ->
            begin match !p with
            | Done v -> continue k v
            | Waiting l -> begin
                p := Waiting (k::l);
                dequeue ()
              end
            end
    in
    fork (ref (Waiting [])) main
end

open Scheduler

let main () =
  let task name () =
    Printf.printf "starting %s\n%!" name;
    let v = Random.int 100 in
    Printf.printf "yielding %s\n%!" name;
    yield ();
    Printf.printf "ending %s with %d\n%!" name v;
    v
  in
  let pa = async (task "a") in
  let pb = async (task "b") in
  let pc = async (fun () -> await pa + await pb) in
  Printf.printf "Sum is %d\n" (await pc);
  assert (await pa + await pb = await pc)

let _ = run main
