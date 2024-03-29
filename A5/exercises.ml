open Async.Std

let job name t =
  Printf.printf "Starting %s\n%!" name;
  (after (Core.Std.sec t)) >>=
  (fun () -> (Printf.printf "Finished %s\n%!" name); return name)

let both d1 d2 =
  d1 >>= (fun a -> (d2 >>= (fun b -> return (a, b))))

let fork d f1 f2 =
  upon d (fun a -> f1 a; ()); upon d (fun a -> f2 a; ())

let parallel_map f l =
  (List.fold_left
    (fun acc a -> (f a) >>= (fun x -> acc >>= (fun bl -> return (x::bl))))
    (return []) l) >>= (fun blist -> return (List.rev blist))

let sequential_map f l =
  let rec helper alst =
    match alst with
    | [] -> return []
    | hd::tl -> (f hd) >>=
      (fun b -> (helper tl) >>= (fun bl -> return (b::bl))) in
  helper l

let any ds =
  let result = Ivar.create () in
  let fill = fun x -> if Ivar.is_empty result then Ivar.fill result x in
  List.fold_left (fun acc a -> upon a fill) () ds;
  Ivar.read result