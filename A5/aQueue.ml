open Async.Std

(** AF: the list [a1; ...; an] represents the queue {a1; ...; an}.
  * The head of the list (a1) represents the top of the queue (oldest item).
  * a1, ..., an are Ivars that can either be empty or filled. A list of all
  * empty Ivars represents an empty queue.
  * The list is captured by a ref to allow mutability
  * RI: The list cannot contain both filled and unfilled Ivars. Unfilled Ivars
  * only occur when we try to pop from an empty queue. Once something is pushed,
  * that Ivar is filled but also removed from the queue.
  *)
type 'a t = 'a Ivar.t list ref

let create () =
  ref []

let is_empty q =
  (q = (create ())) ||
  (List.fold_left (fun acc a -> acc && (a = Ivar.create ())) true (!q))

let is_empty_reflst q = (q = (create ()))

let push q x =
  let packed = Ivar.create () in
  Ivar.fill packed x;
  if is_empty_reflst q then
    q := (!q) @ [packed]
  else
    if Ivar.is_empty (List.hd (!q)) then
      (Ivar.fill (List.hd (!q)) x; q := List.tl (!q))
    else
      q := (!q) @ [packed]

let pop q =
  if is_empty q then
    let empty_ivar = Ivar.create () in
    (q := (!q) @ [empty_ivar]; Ivar.read empty_ivar)
  else
    (let result = Ivar.read (List.hd (!q)) in q := List.tl (!q); result)