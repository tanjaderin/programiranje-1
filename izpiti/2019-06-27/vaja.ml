type complex = {re: float; im :float}

let complex_add x y = {re = x.re +. y.re ; im = x.im +. y.im}
let complex_conjugate (x : complex) (y : complex) = {re = -. x.re ; im = x.im}

let list_apply_either (pred : 'a -> bool) f g xs = 
  let rec pomozna acc xs =
  match xs with
  | [] -> acc
  | x :: xy -> if pred x = true then pomozna (f x :: acc) xy else pomozna (g x :: acc) xy
  in
  pomozna []  xs

  let potenciraj y n = 
    let rec potenciraj' acc y n =
      if n <= 0 then acc else potenciraj'(( y * y) * acc) y (n-1)
    in
    potenciraj' 1 y n

let eval_poly koef a =
  let rec eval' acc koef =
  match koef with 
  |[] -> acc
  | x :: xs -> 
      let stopnja = List.length koef in
      eval' ((x * (potenciraj a stopnja)) + acc) xs
  in
  eval' 0 koef


type najemnik = string
type vrt = 
  |Obdelovan of najemnik
  |Oddan of najemnik * (vrt * vrt list)
  |Prost

let vrt_primer = Oddan ("Kovalevskaya" ,
  (Obdelovan "Galois", Prost :: [Obdelovan "Lagrange"] ))

let obdelovalec_vrta vrt =
  |Prost -> None
  |Oddan -> None
  |Obdelovan x -> Some x

let v_uporabi vrt = 
  |Obdelovan x -> True
  |Prost -> false
  |Oddan (x,( y, ys )) -> v_uprabi x || List.exists v_uporabi ys

let rec vsi_najemniki = function
  | Obdelovan vrtickar -> [vrtickar]
  | Prost -> []
  | Oddan (oddajalec, (podvrt, podvrtovi)) ->
    let vsi_podnajemniki =
      List.fold_left (fun acc vrt -> vsi_najemniki vrt @ acc) [] podvrtovi
    in
    oddajalec :: vsi_najemniki podvrt @ vsi_podnajemniki

let rec vsi_obdelovalci = function
  | Obdelovan vrtickar -> [vrtickar]
  | Prost -> []
  | Oddan (_, (podvrt, podvrtovi)) ->
    List.fold_left
      (fun acc vrt -> vsi_obdelovalci vrt @ acc) [] (podvrt :: podvrtovi)