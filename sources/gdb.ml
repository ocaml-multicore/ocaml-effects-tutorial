effect Peek : int
effect Poke : unit

let rec a i = perform Peek + Random.int i
let rec b i = a i + Random.int i
let rec c i = b i + Random.int i

let rec d i =
  Random.int i +
  match c i with
  | v -> v
  | effect Poke k -> continue k ()

let rec e i =
  Random.int i +
  match d i with
  | v -> v
  | effect Peek k ->
      Printexc.(print_raw_backtrace stdout (get_continuation_callstack k 100));
      flush stdout;
      continue k 42

let _ = Printf.printf "%d\n" (e 100)
