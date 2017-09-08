open Printf

module type Aio = sig
  type 'a promise
  (** Type of promises *)
  val async : (unit -> 'a) -> 'a promise
  (** [async f] runs [f] concurrently *)
  val await : 'a promise -> 'a
  (** [await p] returns the result of the promise. *)
  val yield : unit -> unit
  (** yields control to another task *)

  val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
  val recv   : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int
  val send   : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int

  val run   : (unit -> 'a) -> unit
  (** Runs the scheduler *)
end

module Aio : Aio = struct

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

  type file_descr = Unix.file_descr
  type sockaddr = Unix.sockaddr
  type msg_flag = Unix.msg_flag

  effect Accept : file_descr -> (file_descr * sockaddr)
  let accept fd = perform (Accept fd)

  effect Recv : file_descr * bytes * int * int * msg_flag list -> int
  let recv fd buf pos len mode = perform (Recv (fd, buf, pos, len, mode))

  effect Send : file_descr * bytes * int * int * msg_flag list -> int
  let send fd bus pos len mode = perform (Send (fd, bus, pos, len, mode))

  (********************)

  let ready_to_read fd =
    match Unix.select [fd] [] [] 0. with
    | [], _, _ -> false
    | _ -> true

  let ready_to_write fd =
    match Unix.select [] [fd] [] 0. with
    | _, [], _ -> false
    | _ -> true

  let q = Queue.create ()
  let enqueue t = Queue.push t q

  type blocked = Blocked : 'a eff * ('a, unit) continuation -> blocked

  (* tasks blocked on reads *)
  let br = Hashtbl.create 13
  (* tasks blocked on writes *)
  let bw = Hashtbl.create 13

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
            | Blocked (Accept fd, k) ->
                enqueue (fun () -> continue k (Unix.accept fd))
            | Blocked (Send (fd, buf, pos, len, mode), k) ->
                enqueue (fun () -> continue k (Unix.send fd buf pos len mode))
            | Blocked (Recv (fd, buf, pos, len, mode), k) ->
                enqueue (fun () -> continue k (Unix.recv fd buf pos len mode))
            | Blocked _ -> failwith "impossible"
            end;
            Hashtbl.remove ht x;
            resume ht xs
      in
      resume br rdy_rd_fds;
      resume br rdy_wr_fds;
      schedule ()
    end

  let run main =
    let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit =
      fun pr main ->
        match main () with
        | v ->
            let l = match !pr with Waiting l -> l | _ -> failwith "impossible" in
            List.iter (fun k -> enqueue (fun () -> continue k v)) l;
            pr := Done v;
            schedule ()
        | effect (Async f) k ->
            let pr = ref (Waiting []) in
            enqueue (fun () -> continue k pr);
            fork pr f
        | effect Yield k ->
            enqueue (continue k);
            schedule ()
        | effect (Await p) k ->
            begin match !p with
            | Done v -> continue k v
            | Waiting l -> begin
                p := Waiting (k::l);
                schedule ()
              end
            end
        | effect (Accept fd) k ->
            if ready_to_read fd then
              continue k (Unix.accept fd)
            else begin
              Hashtbl.add br fd (Blocked (Accept fd, k));
              schedule ()
            end
        | effect (Send (fd,buf,pos,len,mode) as e) k ->
            if ready_to_write fd then
              continue k (Unix.send fd buf pos len mode)
            else begin
              Hashtbl.add bw fd (Blocked (e, k));
              schedule ()
            end
        | effect (Recv (fd,buf,pos,len,mode) as e) k ->
            if ready_to_read fd then
              continue k (Unix.recv fd buf pos len mode)
            else begin
              Hashtbl.add br fd (Blocked (e, k));
              schedule ()
            end
    in
    fork (ref (Waiting [])) main
end

module M = Echo.Make(struct
  let accept = Aio.accept
  let recv = Aio.recv
  let send = Aio.send
  let fork f = ignore (Aio.async f)
  let run f = Aio.run f
  let non_blocking_mode = true
end)

let _ = M.start ()
