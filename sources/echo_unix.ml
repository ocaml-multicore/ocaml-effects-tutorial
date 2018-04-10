module M = Echo.Make(struct
  let accept fd = Unix.accept fd
  let recv = Unix.recv
  let send = Unix.send
  let fork f = f ()
  let run f = f ()
  let non_blocking_mode = false
end)

let _ = M.start ()
