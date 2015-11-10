open Async.Std

(**
 * [job name t] simulates a job that takes [t] seconds.
 *
 * It first prints "starting <name>" immediately when called.  After [t]
 * seconds, it should print "finished <name>" and return its name
 *)
val job  : string -> float -> string Deferred.t

(**
 * [both d1 d2] becomes determined with value (a,b) after d1 becomes determined
 * with value a and d2 becomes determined with value b.
 *)
val both : 'a Deferred.t -> 'b Deferred.t -> ('a * 'b) Deferred.t

(**
 * [fork d f g] runs [f] and [g] concurrently when d becomes determined.  The
 * results returned by [f] and [g] are ignored.
 *)
val fork : 'a Deferred.t -> ('a -> 'b Deferred.t)
                         -> ('a -> 'c Deferred.t) -> unit

(**
 * [sequential_map f xs] is similar to [List.map f xs]: it should apply [f] to
 * every element of [xs], and return the list of results.
 *
 * It should wait for each call to f to return
 * before proceeding to the next call.
 *)
val sequential_map : ('a -> 'b Deferred.t) -> 'a list -> 'b list Deferred.t

(**
 * [parallel_map f xs] is similar to [List.map f xs]: it should apply [f] to
 * every element of [xs], and return the list of results.
 *
 * It should call f on every element of xs immediately; it should not wait
 * until one call completes before starting the next.
 *)
val parallel_map : ('a -> 'b Deferred.t) -> 'a list -> 'b list Deferred.t

(**
 * [any xs] becomes determined with value [v] if any of the elements of xs
 * become determined with value [v].
 *)
val any : 'a Deferred.t list -> 'a Deferred.t

