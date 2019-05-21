(* all_true : bool list -> bool *)
let all_true blist =
  if blist = [] then failwith "empty list";
  List.fold_left (fun acc b -> acc && b) true blist
