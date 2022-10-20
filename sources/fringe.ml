(* Same Fringe Problem

   Definition: Two binary trees have the same fringe if they have exactly
   the same leaves reading from left to right.

   Problem: Given two binary trees decide whether they have the same fringe.
*)

type ('elt,'cont) iterator = ('elt -> unit) -> 'cont -> unit

type 'elt generator = unit -> 'elt option

let generate (type elt) (i : (elt, 'container) iterator) (c : 'container) : elt generator =
  let open Effect in
  let open Effect.Shallow in
  let module M = struct
      type _ Effect.t +=
          Yield : elt -> unit Effect.t

      type ('a, 'b) status =
        NotStarted
        | InProgress of ('a,'b) continuation
        | Finished
    end
  in
  let open M in
  let yield v = perform (Yield v) in
  let curr_status = ref NotStarted in
  let rec helper () =
    match !curr_status with
    | Finished -> None
    | NotStarted ->
            curr_status := InProgress (fiber (fun () -> i yield c));
            helper ()
    | InProgress k ->
        continue_with k ()
        { retc = (fun _ ->
                    curr_status := Finished;
                    helper ());
          exnc = (fun e -> raise e);
          effc = (fun (type b) (eff: b Effect.t) ->
              match eff with
              | Yield x -> Some (fun (k: (b,_) continuation) ->
                      curr_status := InProgress k;
                      Some x
                      )
              | _ -> None)}
  in
  helper

type 'a tree =
| Leaf of 'a
| Node of 'a tree * 'a tree

let same_fringe t1 t2 = failwith "not implemented"

let t1 = Node (Leaf 1, Node (Leaf 2, Leaf 3))
let t2 = Node (Node (Leaf 1, Leaf 2), Leaf 3)
let t3 = Node (Node (Leaf 3, Leaf 2), Leaf 1)
let t4 = Leaf 42
let t5 = Leaf 41
let t6 = Node (Leaf 1, Leaf 2)
let t7 = Node (Leaf 1, Node (Leaf 2, Leaf 3))
;;

assert (same_fringe t1 t2);;
assert (same_fringe t2 t1);;
assert (not (same_fringe t1 t3));;
assert (same_fringe t1 t7);;
assert (same_fringe t2 t7);;
