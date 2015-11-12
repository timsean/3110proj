open Exercises
open AQueue
open Async.Std
(******************************************************************************)
(** Unit tests for Exercises **************************************************)
(******************************************************************************)

(** Note: you do not need to write unit tests for job. *)

TEST "both has unit tests" = failwith "TODO"

TEST "fork has unit tests" = failwith "TODO"

TEST "parallel_map has unit tests" = failwith "TODO"

TEST "sequential_map has unit tests" = failwith "TODO"

TEST "any has unit tests" = failwith "TODO"

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


  TEST "pop has unit tests" = failwith "TODO"

  TEST "is_empty has unit tests" = failwith "TODO"
end
