open Async.Std

(** An asynchronous queue containing values of type 'a. *)
type 'a t

(** Create a new queue. *)
val create : unit -> 'a t

(** Add an element to the queue. *)
val push   : 'a t -> 'a -> unit

(** Returns a Deferred that will become determined when an item in the queue is available *)
val pop    : 'a t -> 'a Deferred.t

(** Return true if the queue is empty *)
val is_empty : 'a t -> bool

