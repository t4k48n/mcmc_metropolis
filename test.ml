open Utils

let all_true_tests =
  let t1 = all_true [true; true; true; true] = true in
  let t2 = all_true [true; false; true; true] = false in
  (* an exception is thrown if the argument is an empty list *)
  let t3 = try
      ignore (all_true []);
      false
    with
      | _ -> true
    in
  [t1; t2; t3]
let () = assert (all_true all_true_tests)

open Scratch

let uniform_tests =
  (* sample values from the uniform distribution U[-1, 1] *)
  let samples = List.init 1000000 (fun _ -> uniform (-1.) 1.) in
  (* all the values are included in [-1, 1] *)
  let t1 =
    List.fold_left (fun acc s -> acc && (-1.) <= s && s <= 1.) true samples in
  (* the mean error is less than 5% of the width, that is 2.0 *. 0.05 *)
  let t2 =
    abs_float
        ((List.fold_left (fun acc s -> acc +. s) 0. samples)
            /. float (List.length samples))
      <= 2.0 *. 0.05 in
  [t1; t2]
let () = assert (all_true uniform_tests)

let std_normal_tests =
  (* sample values from the uniform distribution U[-1, 1] *)
  let samples = List.init 1000000 (fun _ -> std_normal ()) in
  (* the mean absolute value is less than 5% of the std.dev., that is 1.0 *. 0.05 *)
  let t1 =
    abs_float
        ((List.fold_left (fun acc s -> acc +. s) 0. samples)
            /. float (List.length samples))
      <= 1.0 *. 0.05 in
  [t1]
let () = assert (all_true std_normal_tests)
