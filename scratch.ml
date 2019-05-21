let pi = 4.0 *. atan 1.0

(* 範囲[a, b]の一様分布からサンプル *)
(* uniform : float -> float -> float *)
let uniform a b =
  let low = if a > b then b else a in
  let d = abs_float (b -. a) in
  Random.float d +. low

(* ガウス分布N(mean, sdev^2)からサンプル *)
(* normal : ~mean:float -> ~sdev:float -> float *)
let normal =
  (* make the buffer for storing a sample because Box-Muller's method generates
   * two samples in a time *)
  let buf = ref None in
  fun ~mean ~sdev ->
    (* obtain a standard sample in N(0, 1^2) *)
    let sample = match !buf with
      | Some(n) -> begin
          buf := None;
          n
        end
      | None -> begin
          let x = uniform 0.0 1.0 in
          let y = uniform 0.0 1.0 in
          let m = sqrt (-. 2. *. log x) *. cos (2. *. pi *. y) in
          let n = sqrt (-. 2. *. log x) *. sin (2. *. pi *. y) in
          buf := Some(n);
          m
        end in
    (* scale the sample from N(0, 1^2) into N(mean, sdev^2) *)
    sample *. sdev +. mean

(* 標準ガウス分布N(0,1^2)からサンプル *)
(* std_normal : unit -> float *)
let std_normal () = normal 0.0 1.0

(* 正規分布N(mean, sdev^2)の確率密度関数 *)
(* pdf_normal : ~mean:float -> ~sdev:float -> float *)
let pdf_normal ~mean ~sdev x =
  let s2 = sdev ** 2. in
  exp (-. ((x -. mean) ** 2. /. (2. *. s2))) /. sqrt (2. *. pi *. s2)

(* likelihood : float list -> float *)
let likelihood ~mean ~sdev = function
  | [] -> failwith "empty list"
  | [_] -> 1.
  | p :: pp :: _ -> begin
      let err = abs_float (pp -. p) in
      pdf_normal ~mean:mean ~sdev:sdev err
    end

(*let update data prev_lh =*)
