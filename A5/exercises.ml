open Async.Std

let job name t =
  Printf.printf "Starting %s\n%!" name;
  (after (Core.Std.sec t)) >>=
  (fun () -> (Printf.printf "Finished %s\n%!" name); return name)

let both d1 d2 =
  d1 >>= (fun a -> (d2 >>= (fun b -> return (a, b))))

let fork d f1 f2 =
  upon d (fun a -> (f1 a); (f2 a); ())

let parallel_map f l =
  failwith "TODO"

let sequential_map f l =
  failwith "TODO"

let any ds =
  failwith "TODO"

