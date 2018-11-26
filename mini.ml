(* -------- 1 -------- *)
let vsota_seznama sez = 
  let rec vsota_acc acc = function 
    | [] -> acc
    | x :: xs -> vsota_acc (x + acc) xs
  in
  vsota_acc 0 sez
(* -------- 2 -------- *)
 let rec urejen_seznam sez = function
  | a :: [] -> true
  | a :: b :: [] -> if a <= b then true else false
  | x :: y :: xs -> if x <= y then urejen_seznam (y :: xs) else false

(* -------- 3 -------- *)
 let rec vstavi x sez = 
  match sez with
  | [] -> [x]
  | y :: ys -> if y <= x then y :: vstavi x ys else x :: y :: xs
(* -------- 4 -------- *)
let rec vstavi2 cmp sez =
  let rec pomozna acc sez =
     match sez with
    | a :: [] -> a :: acc
    | x :: y :: xs -> if cmp xy then pomozna (x :: acc) (y :: ys) else pomozna (y :: acc) (x :: ys)
  in
  pomozna [] sez
(* -------- 5 -------- *)
type priority  =
  |Top
  |Group of int

type status =
  |Staff
  |Passanger of (priority)


type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
]


(* -------- 6 -------- *)
let zaporedje_vkrcanja flyers =
  |match flyers with =
  |x :: xs  -> flyer.status
  |
(* -------- 7 -------- *)
let bloki_potnikov sez =
  let bloki  acc1 acc2 acc3 sez =
    match sez with =
    |