(* -------- 1 -------- *)
let rec vsota sez = 
  let rec vsota' acc sez =
    match sez with
    |[] -> acc
    |x :: xs -> vsota' (x + acc) xs
  in
  vsota' 0 sez
 
(* -------- 2 -------- *)
let rec preveri sez =
  match sez with
  |[] -> true
  | x :: y :: xs when x <= y -> preveri (y :: xs)
  | _ -> false

(* -------- 3 -------- *)
let rec reverse xs =
  let rec reverse' acc xs = 
  match xs with 
  | [] -> acc
  | x :: xs' -> reverse' (x :: acc ) xs'
  in
  reverse' xs []

let rec vstavi stevilo seznam = 
  let rec vstavi' acc stevilo seznam =
  match seznam with 
  | [] -> reverse acc
  | x :: xs when x > stevilo -> vstavi' (x :: stevilo :: acc) stevilo xs
  | x :: xs -> vstavi' (x :: acc) stevilo xs
  in 
  vstavi' [] stevilo seznam

(* -------- 4 -------- *)
(*et rec urejanje cmp sez = *)

(* -------- 5 -------- *)

type priority =
  |Top
  |Group of int

type status =
  |Staff
  |Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)
(*let uredi = function
  |{status tip} *)
(* -------- 7 -------- *)
