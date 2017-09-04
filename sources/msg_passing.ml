effect Xchg : int -> int

(* status of a computation *)
type status =
  Done
| Paused of int * (int, status) continuation

(* step through [f v] until either termination or pauses on Xchg *)
let step f v () =
  match f v with
  | _ -> Done
  | effect (Xchg m) k -> Paused (m, k)

(* Run both of the computations concurrenty *)
let rec run_both a b =
  match a (), b () with
  | Done, Done -> ()
  | Paused (v1, k1), Paused (v2, k2) ->
      run_both (fun () -> continue k1 v2) (fun () -> continue k2 v1)
  | _ -> failwith "improper synchronization"

let rec f name = function
  | 0 -> ()
  | n ->
      Printf.printf "%s: sending %d\n%!" name n;
      let v = perform (Xchg n) in
      Printf.printf "%s: received %d\n%!" name v;
      f name (n-1)

let _ = run_both (step (f "a") 3) (step (f "b") 3)
