open Exercises
open AQueue
open Async.Std
(******************************************************************************)
(** Unit tests for Exercises **************************************************)
(******************************************************************************)

(** Note: you do not need to write unit tests for job. *)
TEST_MODULE "exercise tests" = struct
  (*both tests*)
(*
  TEST "both_immediate" = Thread_safe.block_on_async
    (fun () -> both (return 42) (return 24)) = Core.Std.Result.Ok (42,24)

  TEST "both_wait" = Thread_safe.block_on_async
    (fun () -> both (job "a" 1.0) (job "b" 2.0)) = Core.Std.Result.Ok ("a","b")

  TEST "both_immed_and_wait" = Thread_safe.block_on_async
    (fun () -> both (job "a" 1.0) (return 42)) = Core.Std.Result.Ok ("a",42)*)

  (*fork tests*)
  let wait = job "z" 1.
  let a = ref 0
  let b = ref 0
  let _ = fork wait
    (fun x -> (after (Core.Std.sec 1.)) >>=
      (fun () -> a := 1 ; return x))
    (fun x -> (after (Core.Std.sec 2.)) >>=
      (fun () -> b := 2 ; return x))
  TEST "fork_test_initial_waiting" = Thread_safe.block_on_async
    (fun () -> wait) = Core.Std.Result.Ok "z"
  let wait1 = Thread_safe.block_on_async
    (fun () -> (after (Core.Std.sec 1.))) = Core.Std.Result.Ok ()
  TEST "fork_test_check_ref1" = !a = 1
  let wait2 = Thread_safe.block_on_async
    (fun () -> (after (Core.Std.sec 1.))) = Core.Std.Result.Ok ()
  TEST "fork_test_check_ref2" = !b = 2

  (*map tests*)
  let job_name_list = ["a";"b";"c"]
  let number_list = [1;2;3]
  let ref_list = [ref 1; ref 2; ref 3]

  let parallel_map_immediate = parallel_map (fun x -> return x) number_list
  TEST "parallel_map_immediate" = Thread_safe.block_on_async
    (fun () -> parallel_map_immediate) = Core.Std.Result.Ok number_list

  let parallel_map_wait = parallel_map (fun x -> job x 1.) job_name_list
  TEST "parallel_map_wait" = Thread_safe.block_on_async
    (fun () -> parallel_map_wait) = Core.Std.Result.Ok job_name_list

  let parallel_map_wait_mixed = parallel_map
    (fun x -> if x = "a" then job x 2. else job x 1.) job_name_list
  TEST "parallel_map_wait_mixed" = Thread_safe.block_on_async
    (fun () -> parallel_map_wait_mixed) = Core.Std.Result.Ok job_name_list

  let sequential_map_immediate = sequential_map (fun x -> return x) number_list
  TEST "sequential_map_immediate" = Thread_safe.block_on_async
    (fun () -> sequential_map_immediate) = Core.Std.Result.Ok number_list

  let sequential_map_wait = sequential_map (fun x -> job "wait" 1.) job_name_list
  TEST "sequential_map_wait" = Thread_safe.block_on_async
    (fun () -> sequential_map_wait) = Core.Std.Result.Ok job_name_list

  let sequential_map_wait_mixed = sequential_map
    (fun x -> if x = "a" then job x 2. else job x 1.) job_name_list
  TEST "sequential_map_wait_mixed" = Thread_safe.block_on_async
    (fun () -> sequential_map_wait_mixed) = Core.Std.Result.Ok job_name_list

  (*any tests*)
  let job_list_immediate = [return 1; return 2; return 3]
  TEST "any_immediate" = Thread_safe.block_on_async
    (fun () -> any job_list_immediate) = Core.Std.Result.Ok 1

  let job_list_wait = [job "a" 2.; job "b" 1.; job "c" 3.]
  TEST "any_wait" = Thread_safe.block_on_async
    (fun () -> any job_list_wait) = Core.Std.Result.Ok "b"
end

(******************************************************************************)
(** Unit tests for AQueue *****************************************************)
(******************************************************************************)
(** Note: you do not have to write tests for create *)
TEST_MODULE "queue tests" = struct
  let qtest = create ()

  TEST "push0_isempty" = is_empty qtest
  let _ = push qtest 1
  TEST "push1_isnotempty" = not (is_empty qtest)
  let _ = push qtest 2
  TEST "push2_isnotempty" = not (is_empty qtest)
  let _ = push qtest 3
  TEST "push3_isnotempty" = not (is_empty qtest)
  TEST "pop1" = (pop qtest = (return 1))
  TEST "pop2" = (pop qtest = (return 2))
  TEST "pop3" = (pop qtest = (return 3))
  TEST "popped_all_isempty" = is_empty qtest
  let deferredpop = pop qtest
  TEST "before_async_pop_isempty" = is_empty qtest
  let _ = push qtest 123
  TEST "async_pop" = (deferredpop = (return 123))
  TEST "after_async_pop_isempty" = is_empty qtest

  let defpop1 = pop qtest
  TEST "async_pop1_isempty" = is_empty qtest
  let defpop2 = pop qtest
  TEST "async_pop2_isempty" = is_empty qtest
  let defpop3 = pop qtest
  TEST "async_pop3_isempty" = is_empty qtest
  let _ = push qtest 10
  TEST "async_pop1" = (defpop1 = (return 10))
  TEST "after_async_pop1_isempty" = is_empty qtest
  let _ = push qtest 20
  TEST "async_pop2" = (defpop2 = (return 20))
  TEST "after_async_pop1_isempty" = is_empty qtest
  let _ = push qtest 30
  TEST "async_pop3" = (defpop3 = (return 30))
  TEST "after_async_pop1_isempty" = is_empty qtest
end
