# Concurrent Programming with Effect Handlers 
<!-- [![Build Status](https://travis-ci.org/ocamllabs/ocaml-effects-tutorial.svg?branch=master)](https://travis-ci.org/ocamllabs/ocaml-effects-tutorial)    -->
[![Build Status](https://github.com/ocamllabs/ocaml-effects-tutorial/actions/workflows/prs.yml/badge.svg)](https://github.com/ocamllabs/ocaml-effects-tutorial/actions/workflows/prs.yml)

Originally written as materials for the [CUFP 17 tutorial](http://cufp.org/2017/c3-daniel-hillerstrom-kc-concurrent-programming-with-effect-handlers.html).

## Setting up

### Install a compatible OCaml compiler

Up to date instructions can be found at https://github.com/ocaml-multicore/awesome-multicore-ocaml#installation

### Install required tools

```sh-session
$ opam install ocamlbuild ocamlfind
```


## Outline

The tutorial is structured as follows:

1. [Algebraic Effects and Handlers.](#1-algebraic-effects-and-handlers)  
  1.1. [Recovering from errors](#11-recovering-from-errors)  
  1.2. [Basics](#12-basics)   
2. [Shallow and Deep Handlers.](#2-shallow-and-deep-handlers)    
3. [Delimited Continuations: A deep dive.](#3-delimited-continuations-a-deep-dive)  
  3.1. [Examining effect handlers through GDB](#31-examining-effect-handlers-through-gdb)  
4. [Generators & Streams.](#4-general-control-flow-abstractions-generators--streams)  
  4.1. [Message passing](#41-message-passing)   
  4.2. [Generators from iterators](#42-generators-from-iterators)  
  4.3. [Using the generator](#43-using-the-generator)  
  4.4. [Streams](#44-streams)  
5. [Cooperative Concurrency.](#5-cooperative-concurrency)  
  5.1. [Coroutines](#51-coroutines)  
  5.2. [Async/Await](#52-asyncawait)  
6. [Asynchronous I/O.](#6-asynchronous-io)  
  6.1. [Blocking echo server](#61-blocking-echo-server)  
  6.2. [Asynchronous echo server](#62-asynchronous-echo-server)  
7. [Conclusion.](#7-conclusion)  

The tutorial also includes the following exercises:

1. [Implement exceptions from effects ★☆☆☆☆](#exercise-1-implement-exceptions-from-effects-)
2. [Implement state put and history ★★☆☆☆](#exercise-2-implement-state-put-and-history-)
3. [Derive generator for an arbitrary iterator ★★★★☆](#exercise-3-derive-generator-for-an-arbitrary-iterator-)
4. [Same fringe problem ★☆☆☆☆](#exercise-4-same-fringe-problem-)
5. [Implement async/await functionality ★★★☆☆](#exercise-5-implement-asyncawait-functionality-)
6. [Implement asynchronous accept and send ★☆☆☆☆](#exercise-6-implement-asynchronous-accept-and-send-)

## 1. Algebraic Effects and Handlers

An algebraic effect handler is a programming abstraction for manipulating
control-flow in a first-class fashion. They generalise common abstractions such
as exceptions, generators, asynchronous I/O, or concurrency, as well as other
seemingly esoteric programming abstractions such as transactional memory and
probabilistic computations. 

Operationally, effect handlers offer a form of first-class, restartable exception 
mechanism. In this tutorial, we shall introduce gently algebraic effect and
handlers with gentle examples and then continue on to more involved examples. 

### 1.1. Recovering from errors

Lets start with an example. Consider a program which reads a list of numbers
from standard input and prints the sum of the numbers:

```ocaml
let rec sum_up acc =
    let l = input_line stdin in
    acc := !acc + int_of_string l;
    sum_up acc 

let _ = 
  let r = ref 0 in
  try sum_up r with
  | End_of_file -> Printf.printf "Sum is %d\n" !r
```

The above program is available in `sources/input_line_exn.ml`. You can run this
program as:

```sh-session
$ cd sources
$ ocaml input_line_exn.ml
10
20
(* ctrl+d *)
Sum is 30
```

The `input_line` function returns a string for the input line and raises
`End_of_file` if it encounters end of file character. We use `int_of_string` to
convert the input string to a number. This works as long as the input is a
number. If not, `int_of_string` raises `Failure` and this program blows up:

```sh-session
$ ocaml input_line_exn.ml
10
20
MMXVII
Fatal error: exception Failure("int_of_string")
```

We could print a better error message (`sources/input_line_exn2.ml`):

```ocaml
exception Conversion_failure of string

let int_of_string l =
  try int_of_string l with
  | Failure _ -> raise (Conversion_failure l)

let rec sum_up acc =
    let l = input_line stdin in
    acc := !acc + int_of_string l;
    sum_up acc

let _ =
  let r = ref 0 in
  try sum_up r with
  | End_of_file -> Printf.printf "Sum is %d\n" !r
  | Conversion_failure s ->
      Printf.fprintf stderr "Conversion failure \"%s\"\n%!" s
```

The program now prints a friendlier error message:

```sh-session
$ ocaml input_line_exn2.ml
10
20
MMXVII
Conversion failure "MMXVII"
```

and, unfortunately, the program *terminates*. We really wish the program kept
going:

```ocaml
let _ =
  let r = ref 0 in
  try sum_up r with
  | End_of_file -> Printf.printf "Sum is %d\n" !r
  | Conversion_failure s ->
      Printf.fprintf stderr "Conversion failure \"%s\"\n%!" s
      (* Wish it kept going: continue with 0 *)
```

We could change the code, but if `sum_up` function was from a third-party
library, changing code is generally not an acceptable option. The issue here is
that the library determines whether the error is fatal or not. What we would
like is for **the client of a library determining whether an error is fatal or
not**. 

### 1.2. Basics

Algebraic effect handlers allow you to recover from errors. The following code
is available in `sources/input_line_eff.ml`

```ocaml
open Effect
open Effect.Deep

type _ Effect.t += Conversion_failure : string -> int Effect.t

let int_of_string l =
  try int_of_string l with
  | Failure _ -> perform (Conversion_failure l)

let rec sum_up acc =
    let l = input_line stdin in
    acc := !acc + int_of_string l;
    sum_up acc

let _ =
  Printf.printf "Starting up. Please input:\n%!";
  let r = ref 0 in
  match_with sum_up r
  { effc = (fun (type c) (eff: c Effect.t) ->
      match eff with
      | Conversion_failure s -> Some (fun (k: (c,_) continuation) ->
              Printf.fprintf stderr "Conversion failure \"%s\"\n%!" s;
              continue k 0)
      | _ -> None
    );
    exnc = (function
        | End_of_file -> Printf.printf "Sum is %d\n" !r
        | e -> raise e
    );
    (* Shouldn't reach here, means sum_up returned a value *)
    retc = fun _ -> failwith "Impossible, sum_up shouldn't return"
  }
```

First, lets run this program:

```sh-session
$ ocaml input_line_eff.ml
10
20
MMXVII
Conversion failure "MMXVII"
30
(* ctrl+d *)
Sum is 60
```

We've recovered from the conversion error and kept going. Algebraic effects and
handlers are similar to exceptions in that we can declare new effects:

```ocaml
type _ Effect.t += Conversion_failure : string -> int Effect.t
(* c.f. [exception Conversion_failure of string] *)
```

Effects are declared by adding constructors to an [extensible variant type](https://v2.ocaml.org/manual/extensiblevariants.html)
defined in the `Effect` module.

Unlike exceptions, performing an effect returns a value. The declaration here
says that `Conversion_failure` is an algebraic effect that takes a string
parameter, which when performed, returns an integer. 

Just like exceptions, effects are values. The type of `Conversion_failure
"MMXVII"` is `int Effect.t`, where `int` is the result of performing the effect.
We perform the effect with `perform : 'a Effect.t -> 'a` primitive (c.f. `raise :
exn -> 'a (* bottom *)`). 

Effect handlers are defined in the modules `Effect.Deep` and `Effect.Shallow`.
We'll discuss the differences between the two later.

```ocaml
module Deep : sig
  (** Some contents omitted *)

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c t -> (('c,'b) continuation -> 'b) option }
  (** [('a,'b) handler] is a handler record with three fields -- [retc]
      is the value handler, [exnc] handles exceptions, and [effc] handles the
      effects performed by the computation enclosed by the handler. *)

  val match_with: ('c -> 'a) -> 'c -> ('a,'b) handler -> 'b
  (** [match_with f v h] runs the computation [f v] in the handler [h]. *)

  type 'a effect_handler =
    { effc: 'b. 'b t -> (('b, 'a) continuation -> 'a) option }
  (** ['a effect_handler] is a deep handler with an identity value handler
      [fun x -> x] and an exception handler that raises any exception
      [fun e -> raise e]. *)

  val try_with: ('b -> 'a) -> 'b -> 'a effect_handler -> 'a
  (** [try_with f v h] runs the computation [f v] under the handler [h]. *)
end

module Shallow : sig
  (** Some contents omitted *)

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c t -> (('c,'a) continuation -> 'b) option }
  (** [('a,'b) handler] is a handler record with three fields -- [retc]
      is the value handler, [exnc] handles exceptions, and [effc] handles the
      effects performed by the computation enclosed by the handler. *)

  val continue_with : ('c,'a) continuation -> 'c -> ('a,'b) handler -> 'b
  (** [continue_with k v h] resumes the continuation [k] with value [v] with
      the handler [h].
      @raise Continuation_already_resumed if the continuation has already been
      resumed.
   *)
end
```

The handlers are records with three fields and are called in the context of `match_with`, `try_with`, or `continue_with`:

`retc` is the function that is called when the computation returns a value -
i.e. no effects or exceptions were performed/raised in the computation. The
function has one parameter: the value of the computation

`exnc` is called when the computation throws an exception. It takes the exception as a parameter.

`effc` is the function that handles the effects. It has type `'c. 'c Effect.t -> ('c, 'a) continuation -> 'b) option`

Effects are strongly typed, but the handler function can handle multiple
effects and has to be generic over every possible type (which is potentially
all of them since the effects variant can always be extended further), hence
the `'c` existential type. `effc` returns an `option` where a None value means
ignore the effect (and crash the program if not handled somewhere else). A Some
value holds a function that takes a parameter commonly called `k`

```ocaml
  { effc = (fun (type c) (eff: c Effect.t) ->
      match eff with
      | Conversion_failure s -> Some (fun (k: (c,_) continuation) ->
              Printf.fprintf stderr "Conversion failure \"%s\"\n%!" s;
              continue k 0)
      | _ -> None
    )
   }
```

We need to declare a [locally abstract type](https://v2.ocaml.org/manual/locallyabstract.html) `c` in order to
tell the compiler that `eff` and `k` are constrained on the same type.

The parameter `k`, is the *delimited continuation*
between the point of performing the effect and the effect handler. The delimited
continuation is like a dynamically defined function, that can be called and
returns a value. The type of `k` in this case is `(int, int) continuation`,
which says that the continuation expects an integer to continue (the first type
parameter), and returns with an integer (the second type parameter). 

The delimited continuation is resumed with `Effect.Deep`'s `continue : ('a,'b) continuation ->
'a -> 'b`. In this example, `continue k 0` resumes the continuation
with `0`, and the corresponding `perform (Conversion_failure l)` returns with
`0`.

If we do want to consider the error to be fatal (`sources/input_line_eff2.ml`),
then we can `discontinue : ('a,'b) continuation -> exn -> 'b` the continuation
so that it raises an exception at the perform point. 

```ocaml
  match_with sum_up r
  { effc = (fun (type a) (e: a t) ->
      match e with
      | Conversion_failure s -> Some (fun (k: (a,_) continuation) ->
          Printf.fprintf stderr "Conversion failure \"%s\"\n%!" s;
          discontinue k (Failure "int_of_string"))
      | _ -> None
    );
    exnc = (function
        | End_of_file -> Printf.printf "Sum is %d\n" !r
        | e -> raise e
    );
    (* Shouldn't reach here, means sum_up returned a value *)
    retc = fun v -> v
  }
```

Now,

```sh-session
$ ocaml input_line_eff2.ml
10
20
MMXVII
Conversion failure "MMXVII"
Fatal error: exception Failure("int_of_string")
```

#### 1.2.1. Effects are unchecked

Unlike [Eff](http://www.eff-lang.org/),
[Koka](https://github.com/koka-lang/koka),
[Links](https://github.com/links-lang/links), and other languages that
support effect handlers, effects in Multicore OCaml are unchecked 
currently. A program that does not handle a performed effect fails 
with a runtime error.

Let's fire up the OCaml top-level:

```ocaml
$ ocaml
OCaml version 5.0.0~beta1

# open Effect;;
# type _ Effect.t += E : unit Effect.t;;
type _ Stdlib.Effect.t += E : unit Effect.t
# let f () = perform E;;
val f : unit -> unit = <fun>
# f ();;
Exception: Stdlib.Effect.Unhandled(E)
# open Effect.Deep;;
# try_with f () {effc = (fun (type c) (eff: c Effect.t) ->
      match eff with
      | E -> Some (fun (k: (c,_) continuation) -> continue k ())
      | _ -> None
  )};;
- : unit = ()
```

### Exercise 1: Implement exceptions from effects ★☆☆☆☆

As mentioned before, effects generalise exceptions. Exceptions handlers are
effect handlers that ignore the continuation. Your task is to implement
exceptions in terms of effects. The source file is `sources/exceptions.ml`.

## 2. Shallow vs Deep Handlers

The OCaml standard library provides two different modules for handling effects: `Effect.Deep` and `Effect.Shallow`. When a deep handler returns a continuation, the continuation also includes the handler. This means that, when the continuation is resumed, the effect handler is automatically re-installed, and will handle the effect(s) that the computation may perform in the future.

Shallow handlers on the other hand, allow us to change the handlers every time an effect is performed. Let's use them to implement state without refs. The implementation is available in `sources/state1.ml`.

```ocaml
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
```

We use `Effect.Shallow` by wrapping calculations with `continue_with : ('c,'a) continuation -> 'c -> ('a,'b) handler -> 'b` and getting an initial continuation with `val fiber : ('a -> 'b) -> ('a, 'b) continuation`

In this example, we define an effect `Get` that returns a
value of type `t` when performed. 

```ocaml
module IS = State (struct type t = int end)
module SS = State (struct type t = string end)

let foo () : unit =
  printf "%d\n" (IS.get ());
  printf "%d\n" (IS.get ());
  printf "%d\n" (IS.get ());
  printf "%s\n" (SS.get ());
  printf "%s\n" (SS.get ()) 

let _ = IS.run (fun () -> SS.run foo "forty two") 42
```

We instantiate two state instances, one with an integer type and
another with string type. Running the program returns:

```sh-session
$ ocaml state1.ml
42
42
42
forty two
forty two
```

### Exercise 2: Implement state put and history ★★☆☆☆

Your task it to implement `put : t -> unit` that updates the state and `history
: unit -> t list` that returns the list of values put. Do not use references.
The source file is `sources/state2.ml`.

## 3. Delimited Continuations: A deep dive

**EDITOR'S NOTE: The implementation has changed since this section was written. Results in gdb will differ, but the concepts of the implementation remain mostly the same.**

Algebraic effect handlers in Multicore OCaml are very efficient due to several
choices we make in their implementation. Understanding the implementation of
delimited continuations also helps to develop a mental model for reasoning about
programs that use effect handlers.

Delimited continuations that appear in the effect handler are implemented on top
of **fibers** -- small, heap allocated stack chunks, that grow and shrink on
demand. The execution stack is really a stack of fibers.

```
Execution stack
---------------

+----+   +----+
|    |   |    |
| f1 |<--| f2 |
|    |   |    |<- stack_pointer
+----+   +----+
```

An effect handler instantiates a new fiber for evaluating the expression. 

```
try ex with
| effect e k -> ....

Execution stack
---------------

+----+   +----+    +----+
|    |   |    |    |    |
| f1 |<--| f2 | <--| ex |
|    |   |    |    |    |<- stack_pointer
+----+   +----+    +----+
```

Performing an effect may pop one or more of the fibers based on which handler
handles the effect. The popped sequence of fibers becomes the delimited
continuation.

```
effect E : unit 

try perform E with
| effect E k -> ....

Execution stack
---------------

+----+   +----+                                 +----+
|    |   |    |---k (delimited continuation)--->|    |
| f1 |<--| f2 |                                 | ex |
|    |   |    |<- stack_pointer                 |    |
+----+   +----+                                 +----+
```

When you resume the delimited continuation (with `continue` or `discontinue`)
the fiber sequence that represents the delimited continuation are push on top of
the execution stack. Importantly, our continuations are **one-shot** -- they can
only be resumed once. One shotness means that we never have to copy our
continuations in the case that we may need it for a future invocation. For this
reason, context switching between fibers is really fast and is completely in
userland code and the kernel is not involved. 

### 3.1 Examining effect handlers through GDB

The file `sources/gdb.ml`:

```ocaml
open Effect
open Effect.Deep

type _ Effect.t += Peek : int Effect.t
                 | Poke : unit Effect.t

let rec a i = perform Peek + Random.int i
let rec b i = a i + Random.int i
let rec c i = b i + Random.int i

let rec d i =
  Random.int i +
  try_with c i
  { effc = fun (type a) (e: a t) ->
      match e with
      | Poke -> Some (fun (k: (a,_) continuation) -> continue k ())
      | _ -> None
  }

let rec e i =
  Random.int i +
  try_with d i
  { effc = fun (type a) (e: a t) ->
      match e with
      | Peek -> Some (fun (k: (a,_) continuation) ->
          Printexc.(print_raw_backtrace stdout (Effect.Deep.get_callstack k 100));
          flush stdout;
          continue k 42
        )
      | _ -> None
  }

let _ = Printf.printf "%d\n" (e 100)
```

illustrates the effect handler stack. Let us compile and examine the file under
GDB:

```sh-session
$ make gdb.native 
$ gdb ./gdb.native 
```

`caml_resume` is the native stub function through which a fiber is attached to
the top of the execution stack and control switches to it. This happens when a
new handler is installed, a continuation is resumed with `continue` or
`discontinue`. Similarly `caml_perform` is the native function which implements
`perform` primitive. We set breakpoints on these two functions to observe the
program as it executes. 

```
(gdb) break caml_perform
Breakpoint 1 at 0xaeca8
(gdb) break caml_resume
Breakpoint 2 at 0xaed38
(gdb) r
Starting program: /home/sudha/ocaml/temp/ocaml-effects-tutorial/sources/gdb.native 
[Thread debugging using libthread_db enabled]
Using host libthread_db library "/lib/x86_64-linux-gnu/libthread_db.so.1".

Breakpoint 1, 0x0000555555602ca8 in caml_perform ()
(gdb) bt
#0  0x0000555555602ca8 in caml_perform ()
#1  0x00005555555a3c08 in camlGdb__b_311 () at gdb.ml:7
#2  0x00005555555a3c69 in camlGdb__c_313 () at gdb.ml:9
#3  <signal handler called>
#4  0x00005555555a3cd8 in camlGdb__d_315 () at gdb.ml:13
#5  <signal handler called>
#6  0x00005555555a3db8 in camlGdb__e_329 () at gdb.ml:22
#7  0x00005555555a4034 in camlGdb__entry () at gdb.ml:33
#8  0x00005555555a13ab in caml_program ()
#9  <signal handler called>
#10 0x000055555560252f in caml_startup_common (argv=0x7fffffffda68, pooling=<optimized out>) at runtime/startup_nat.c:129
#11 0x000055555560257b in caml_startup_exn (argv=<optimized out>) at runtime/startup_nat.c:136
#12 caml_startup (argv=<optimized out>) at runtime/startup_nat.c:141
#13 0x00005555555a108c in main (argc=<optimized out>, argv=<optimized out>) at runtime/main.c:37
```

Enter effect handler in `e`. The `<signal handler called>` frames correspond to
the transition between C frames to OCaml frames, and between OCaml frames of two
different fibers. These signal handler frames have nothing to do with signals,
but are just a hack to let GDB know that the execution stack is a linked list of
contiguous stack chunks. 

```
(gdb) c
Continuing.
Raised by primitive operation at Gdb.a in file "gdb.ml" (inlined), line 7, characters 14-26
Called from Gdb.b in file "gdb.ml", line 8, characters 14-17
Called from Gdb.c in file "gdb.ml", line 9, characters 14-17
Called from Gdb.d in file "gdb.ml", line 13, characters 2-159

Breakpoint 2, 0x0000555555602d38 in caml_resume ()
(gdb) bt
#0  0x0000555555602d38 in caml_resume ()
#1  0x00005555555a3db8 in camlGdb__e_329 () at gdb.ml:22
#2  0x00005555555a4034 in camlGdb__entry () at gdb.ml:33
#3  0x00005555555a13ab in caml_program ()
#4  <signal handler called>
#5  0x000055555560252f in caml_startup_common (argv=0x7fffffffda68, pooling=<optimized out>) at runtime/startup_nat.c:129
#6  0x000055555560257b in caml_startup_exn (argv=<optimized out>) at runtime/startup_nat.c:136
#7  caml_startup (argv=<optimized out>) at runtime/startup_nat.c:141
#8  0x00005555555a108c in main (argc=<optimized out>, argv=<optimized out>) at runtime/main.c:37
```

The control switches to the effect handler. In the effect handler for `Peek` in
`e`, we get the backtrace of the continuation and print it.

This break point corresponds to `continue k 42` in `e`.

The program terminates normally. 

```
Continuing.
329
[Inferior 1 (process 8464) exited normally]
```

## 4. Generators & streams.

So far we've seen examples where the handler discards the continuation
(exceptions) and immediately resumes the computation (state). Since the
continuations are first-class values, we can also keep them around and resume
them later, while executing some other code in the mean time. This functionality
allows us to implement programming idioms such as generators, async/await, etc.

### 4.1. Message passing

Let us being with a simple example that illustrates control switching between
two tasks. The two tasks run **cooperatively**, sending messages
between each other. The source code is available in `sources/msg_passing.ml`.

We define an effect `Xchg : int -> int` for exchanging integer messages with the
other task. During an exchange, the task sends as well as receives an integer.

```ocaml
type _ Effect.t += Xchg : int -> int Effect.t
```

Since the task may suspend, we need a way to represent the status of the task:

```ocaml
type status =
  Done
| Paused of int * (int, status) continuation
```

The task may either have been `Done` or is `Paused` with the message to send as
well as the continuation, which expects the message to receive. The continuation
results in another status when resumed. We define a `step` function that runs
the function `f` for one step with argument `v`. 

```ocaml
let step f v () =
  match_with f v
  { retc = (fun _ -> Done);
    exnc = (fun e -> raise e);
    effc = (fun (type b) (eff: b t) ->
        match eff with
        | Xchg m -> Some (fun (k: (b,_) continuation) ->
                Paused (m, k))
        | _ -> None
    )}
```

The task may perform an `Xchg` in which case we return its `Paused` state. We
now define a `run_both` function for running two tasks concurrently.

```ocaml
let rec run_both a b =
  match a (), b () with
  | Done, Done -> ()
  | Paused (v1, k1), Paused (v2, k2) ->
      run_both (fun () -> continue k1 v2) (fun () -> continue k2 v1)
  | _ -> failwith "improper synchronization"
```

Both of the tasks may run to completion, or both may offer to exchange a
message. We consider the other cases to be incorrect programs. In the latter
case, we resume both of the computations with the value from the other.

```ocaml
let rec f name = function
  | 0 -> ()
  | n ->
      Printf.printf "%s: sending %d\n%!" name n;
      let v = perform (Xchg n) in
      Printf.printf "%s: received %d\n%!" name v;
      f name (n-1)

let _ = run_both (step (f "a") 3) (step (f "b") 3)
```

Finally, we test the program with a simple test.

```sh-session
$ ocaml msg_passing.ml
a: sending 3
b: sending 3
a: received 3
a: sending 2
b: received 3
b: sending 2
a: received 2
a: sending 1
b: received 2
b: sending 1
a: received 1
b: received 1
```

### 4.2. Generators from iterators

Iterator is a mechanism to traverse a data structure that retains the control of
the traversal on the library side. An example is `List.iter :  ('a -> unit) ->
'a list -> unit` that applies the given function to every element in the list.
We can provide the following general type for iterators:

```ocaml
type ('elt,'container) iterator = ('elt -> unit) -> 'container-> unit
```

where `'elt` is the type of element and `'container` is the type of the container
over which the function iterates. 

On the other hand, a generator is a function where the client of the library has
control over the traversal. We can imagine a `List.generator : 'a list -> (unit
-> 'a option)` that returns a function, which when called returns the next
element in the list. The function returns `None` if there are no more elements.
We can provide the following general type for generator:

```ocaml
type 'elt generator = unit -> 'elt option
```

Several languages, including Python and JavaScript, provide generators as a
primitive mechanism, usually through an `yield` primitive. Typically, the
functions that can yield require special annotations (such as `function*`) in
JavaScript, and only yield values to the immediate caller.

As we've seen in the earlier example, algebraic effect handlers provide a
mechanism to suspend **arbitrary** computation and capture it in the
continuation. Hence, we can derive the generator for an arbitrary iterator
function.

### Exercise 3: Derive generator for an arbitrary iterator ★★★★☆

Your task is to implement the function `generate : ('elt, 'container) iterator ->
'elt generator` which derives the generator for any iterator function.

Hint: Since calling the generator function is an effectful operation, you might
think about saving the state of the traversal in a reference.

### 4.3. Using the generator

#### 4.3.1. Traversal

You can use the `generator` to traverse a data structure on demand.

```ocaml
$ ocaml
# #use "generator.ml";;
# let gl = generate List.iter [1;2;3];;
val gl : int generator = <fun>
# gl();;
- : int option = Some 1
# gl();;
- : int option = Some 2
# gl();;
- : int option = Some 3
# gl();;
- : int option = None
# let ga = generate Array.iter [| 1.0; 2.0; 3.0 |];;
# ga();;
- : float option = Some 1.
# ga();;
- : float option = Some 2.
# ga();;
- : float option = Some 3.
# ga();;
- : float option = None
```

#### Exercise 4: Same fringe problem ★☆☆☆☆

Two binary trees have the same fringe if they have exactly the same leaves
reading from left to right. Given two binary trees decide whether they have the
same fringe. The source file is `sources/fringe.ml`.

### 4.4. Streams

The iterator need not necessarily be defined on finite data structure. Here is
an iterator that iterates over infinite list of integers.

```ocaml
let rec nats : int (* init *) -> (int, unit) iterator =
  fun v f () ->
    f v; nats (v+1) f ()
```

Since the iteration is not over any particular container, the container type is
`unit`. We can make a generator over this iterator, which yields an infinite
sequence of integers.

```ocaml
let gen_nats : int generator = generate (nats 0) ()
```

We know that this generator does not terminate. Hence, the optional return type
of generator is unnecessary. Hence, we define a type `'a stream` for infinite
streams:

```ocaml
type 'a stream = unit -> 'a
```

We can convert a generator to a stream easily:

```ocaml
let inf : 'a generator -> 'a stream  =
  fun g () ->
    match g () with
    | Some n -> n
    | _ -> assert false
```

Now, an infinite stream of integers starting from 0 is:

```ocaml
let gen_nats : int stream = inf (generate (nats 0) ());;
assert (0 = gen_nats ());;
assert (1 = gen_nats ());;
assert (2 = gen_nats ());;
assert (3 = gen_nats ());;
(* and so on *)
```

We can define operators over the stream such as `map` and `filter`:

```ocaml
let rec filter : 'a stream -> ('a -> bool) -> 'a stream =
  fun g p () ->
    let v = g () in
    if p v then v
    else filter g p ()

let map : 'a stream -> ('a -> 'b) -> 'b stream =
  fun g f () -> f (g ())
```

We can manipulate the streams using these operators. For example,

```ocaml
(* Even stream *)
let gen_even : int stream =
  let nat_stream = inf (generate (nats 0) ()) in
  filter nat_stream (fun n -> n mod 2 = 0)
;;

assert (0 = gen_even ());;
assert (2 = gen_even ());;
assert (4 = gen_even ());;
assert (6 = gen_even ());;

(* Odd stream *)
let gen_odd : int stream =
  let nat_stream = inf (generate (nats 1) ()) in
  filter nat_stream (fun n -> n mod 2 == 1)
;;


assert (1 = gen_odd ());;
assert (3 = gen_odd ());;
assert (5 = gen_odd ());;
assert (7 = gen_odd ());;

(* Primes using sieve of Eratosthenes *)
let gen_primes =
  let s = inf (generate (nats 2) ()) in
  let rs = ref s in
  fun () ->
    let s = !rs in
    let prime = s () in
    rs := filter s (fun n -> n mod prime != 0);
    prime

assert ( 2 = gen_primes ());;
assert ( 3 = gen_primes ());;
assert ( 5 = gen_primes ());;
assert ( 7 = gen_primes ());;
assert (11 = gen_primes ());;
assert (13 = gen_primes ());;
assert (17 = gen_primes ());;
assert (19 = gen_primes ());;
assert (23 = gen_primes ());;
assert (29 = gen_primes ());;
assert (31 = gen_primes ());;
```

## 5. Cooperative Concurrency

OCaml has two popular libraries for cooperative concurrency: Lwt and Async. Both
libraries achieve concurrency through a [concurrency
monad](https://www.seas.upenn.edu/~cis552/11fa/lectures/concurrency.html). As a
result, the programs that wish to use these libraries have to be written in
monadic style. With effect handlers, the code could be written in direct style
but also retain the benefit of asynchronous I/O. While the resultant system
closely resembles [Goroutines in Go](https://tour.golang.org/concurrency/1),
with effect handlers, all of this is implemented in OCaml as a library.

### 5.1. Coroutines

Let us begin with a simple cooperative scheduler. The source code is available
in `sources/cooperative.ml`. The interface we'll implement first is:

```ocaml
module type Scheduler = sig
  val async : (unit -> 'a) -> unit
  (** [async f] runs [f] concurrently *)
  val yield : unit -> unit
  (** yields control to another task *)
  val run   : (unit -> 'a) -> unit
  (** Runs the scheduler *)
end
```

We declare effects for `async` and `yield`:

```ocaml
type _ Effect.t += Async : (unit -> 'a) -> unit Effect.t
               | Yield : unit Effect.t

let async f = perform (Async f)

let yield () = perform Yield
```

We use a queue for the tasks that are ready to run:

```ocaml
let q = Queue.create ()
let enqueue t = Queue.push t q
let dequeue () = 
  if Queue.is_empty q then () 
  else Queue.pop q ()
```

And finally, the main function is:

```ocaml
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
```

If the task runs to completion (value case), then we dequeue and run the next
task from the scheduler. In the case of an `Async f` effect, the current task is
enqueued and the new task `f` is run. If the scheduler yields, then the current
task is enqueued and some other task is dequeued and run from the scheduler. We
can now write a cooperative concurrent program:

```ocaml
let main () =
  let mk_task name () =
    printf "starting %s\n%!" name;
    yield ();
    printf "ending %s\n%!" name
  in
  async (mk_task "a");
  async (mk_task "b")

let _ = run main
```

```sh-session
$ ocaml cooperative.ml
starting a
starting b
ending a
ending b
```

### 5.2. Async/await

We can extend the scheduler to implement async/await idiom. The interface we
will implement is:

```ocaml
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
```

We model a promise as a mutable reference that either is the list of tasks
waiting on this promise to resolve (`Waiting`) or a resolved promise with the
value (`Done`).

```ocaml
type 'a _promise =
  Waiting of ('a,unit) continuation list
| Done of 'a

type 'a promise = 'a _promise ref
```

### Exercise 5: Implement async/await functionality ★★★☆☆

In this task, you will implement the core async/await functionality. Unlike the
previous scheduler, additional work has to be done at the `Async` handler case
to create the promise, and at task termination (value case) to update the
promise and resume the waiting threads. In addition, the `Await` case needs to
be implemented. The source file is `sources/async_await.ml`.

## 6. Asynchronous I/O

Effect handlers lets us write asynchronous I/O libraries in direct-style. 

### 6.1. Blocking echo server

As an example, `sources/echo.ml` is a implementation of an echo server that
accepts client messages and echoes them back. Observe that all of the code is
written in direct, function-calling, and apparently blocking style. We will see
that the same code can be used to implement a blocking as well as non-blocking
server. A non-blocking server can concurrently host multiple client sessions
unlike the blocking server which serialises client sessions.

The echo server is functorized over the network interface:

```ocaml
module type Aio = sig
  val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
  val recv   : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int
  val send   : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int
  val fork   : (unit -> unit) -> unit
  val run    : (unit -> unit) -> unit
  val non_blocking_mode : bool
  (* Are the sockets non-blocking *)
end
```

We can satisfy this interface with functions from the `Unix` module:

```ocaml
struct
  let accept = Unix.accept
  let recv = Unix.recv
  let send = Unix.send
  let fork f = f ()
  let run f = f ()
  let non_blocking_mode = false
end
```

You can test this echo server as follows:

```sh-session
$ make echo_unix.native
$ ./echo_unix.native
Echo server listening on 127.0.0.1:9301
```

In another terminal, establish a client connection:

```sh-session
(* first client *)
$ telnet localhost 9301
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
hello
server says: hello
world
server says: world
```

The server echoes whatever message that is sent. In another terminal, establish
a second concurrent client connection:

```sh-session
(* second client *)
$ telnet localhost 9301
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
hello
world
```

The server does not echo the messages since it is blocked serving the first
client. Now, switch to the first client terminal, and terminate the connection:

```sh-session
(* first client *)
^]
telnet> (* ctrl+d *)
$
```

At this point, you should see that all of the messages sent by the second client
has been echoed:

```sh-session
(* second client *)
server says: hello
server says: world
```

and further messages from the second client are immediately echoed.

### 6.2. Asynchronous echo server

We will extend our async/await implementation to support asynchronous I/O
operations. The source file is `sources/echo_async.ml`. As usual, we declare the
effects and functions to perform the effects:

```ocaml
type file_descr = Unix.file_descr
type sockaddr = Unix.sockaddr
type msg_flag = Unix.msg_flag

type _ Effect.t += Accept : file_descr -> (file_descr * sockaddr) Effect.t
let accept fd = perform (Accept fd)

type _ Effect.t += Recv : file_descr * bytes * int * int * msg_flag list -> int Effect.t
let recv fd buf pos len mode = perform (Recv (fd, buf, pos, len, mode))

type _ Effect.t += Send : file_descr * bytes * int * int * msg_flag list -> int Effect.t
let send fd bus pos len mode = perform (Send (fd, bus, pos, len, mode))
```

We define functions to poll whether a file descriptor is ready to read or write:

```ocaml
let ready_to_read fd =
  match Unix.select [fd] [] [] 0. with
  | [], _, _ -> false
  | _ -> true

let ready_to_write fd =
  match Unix.select [] [fd] [] 0. with
  | _, [], _ -> false
  | _ -> true
```

We define a type for tasks blocked on I/O, and a pair of hash tables to hold the
continuations blocked on reads and writes:

```ocaml
type blocked = Blocked : 'a eff * ('a, unit) continuation -> blocked

(* tasks blocked on reads *)
let br = Hashtbl.create 13
(* tasks blocked on writes *)
let bw = Hashtbl.create 13
```

Now, the handler for `Recv` is:

```ocaml
| effect (Recv (fd,buf,pos,len,mode) as e) k ->
    if ready_to_read fd then
      continue k (Unix.recv fd buf pos len mode)
    else begin
      Hashtbl.add br fd (Blocked (e, k));
      schedule ()
    end
```

If the file descriptor is ready to be read, then we perform the read immediately
with the blocking read form `Unix` module knowing that the read would not block.
If not, we add the task to the blocked-on-read hash table `br`, and schedule the
next task. The main schedule loop is:

```ocaml
  let rec schedule () =
    if not (Queue.is_empty q) then
      (* runnable tasks available *)
      Queue.pop q ()
    else if Hashtbl.length br = 0 && Hashtbl.length bw = 0 then
      (* no runnable tasks, and no blocked tasks => we're done. *)
      ()
    else begin (* no runnable tasks, but blocked tasks available *)
      let rd_fds = Hashtbl.fold (fun fd _ acc -> fd::acc) br [] in
      let wr_fds = Hashtbl.fold (fun fd _ acc -> fd::acc) bw [] in
      let rdy_rd_fds, rdy_wr_fds, _ = Unix.select rd_fds wr_fds [] (-1.) in
      let rec resume ht = function
        | [] -> ()
        | x::xs ->
            begin match Hashtbl.find ht x with
            | Blocked (Recv (fd, buf, pos, len, mode), k) ->
                enqueue (fun () -> continue k (Unix.recv fd buf pos len mode))
            | Blocked (Accept fd, k) -> failwith "not implemented"
            | Blocked (Send (fd, buf, pos, len, mode), k) -> failwith "not implemented"
            | Blocked _ -> failwith "impossible"
            end;
            Hashtbl.remove ht x
      in
      resume br rdy_rd_fds;
      resume br rdy_wr_fds;
      schedule ()
    end
```

The interesting case is when runnable tasks are not available and there are
blocked tasks. In this case, we run an iteration of the event loop. This
may unblock further tasks and we continue running them.


### Exercise 6: Implement asynchronous accept and send ★☆☆☆☆

In the file, `sources/echo_async.ml`, some of the functionality for handling
`Accept` and `Send` event are not implemented. Your task is to implement these.
Once you implement these primitives, you can run `echo_async.native` to start
the asynchronous echo server. This server is able to respond to multiple
concurrent clients.

## 7. Conclusion

Hopefully you've enjoyed the tutorial on algebraic effect handlers in Multicore
OCaml. You should be familiar with:

  * What algebraic effects and handlers are.
  * Programming with algebraic effect handlers in Multicore OCaml.
  * Implementation of algebraic effect handlers in Multicore OCaml.
  * Developing control-flow abstractions such as restartable exceptions,
    generators, streams, coroutines, and asynchronous I/O.


### 7.1 Other resources

  * [OCaml manual on Effects and handlers](https://kcsrk.info/webman/manual/effects.html)
  * [effect.mli](https://github.com/ocaml/ocaml/blob/trunk/stdlib/effect.mli) in OCaml standard library
