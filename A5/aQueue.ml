open Async.Std

type 'a t = 'a Ivar.t list ref

let create () =
  ref []

let is_empty q =
  q = (create ())

let push q x =
  let packed = Ivar.create () in
  Ivar.fill packed x;
  if is_empty q then
    q := (!q) @ [packed]
  else
    if Ivar.is_empty (List.hd (!q)) then
      (Ivar.fill (List.hd (!q)) x; q := [])
    else
      q := (!q) @ [packed]

let pop q =
  if is_empty q then
    (q := (!q) @ [Ivar.create ()]; Ivar.read (List.hd (!q)))
  else
    (let result = Ivar.read (List.hd (!q)) in q := List.tl (!q); result)