
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [square] vrne kvadrat podanega celega števila.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # square 2;;
 - : int = 4
[*----------------------------------------------------------------------------*)


let rec last_element seznam =
  match seznam with
  | [] -> failwith "prekratko"
  | x :: [] -> x
  | y :: ys -> last_element (ys)


let rec penultimate_element = function
    | [] | _ :: []-> failwith "prekratko"
    | x :: _ :: [] -> x
    | _ :: y :: ys -> penultimate_element (y :: ys)

let rec penultimate_element seznam =
  match seznam with
  | [] -> failwith "prekratko"
  | x :: [] -> failwith "prekratko"
  | x :: y :: [] -> x
  | x :: y :: ys -> penultimate_element (y :: ys)


let square n = n * n
  

(*----------------------------------------------------------------------------*]
 Funkcija [middle_of_triple] vrne srednji element trojice.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # middle_of_triple (true, false, true);;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec middle_of_triple (_, b, _) = b

(*----------------------------------------------------------------------------*]
 Funkcija [starting_element] vrne prvi element danega seznama. V primeru
 prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # starting_element [1; 2; 3; 4];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec starting_element = function
  | [] -> failwith "prekratko"
  | x :: _ -> x

let starting_element seznam = 
  match seznam with
    | [] -> failwith "kratko"
    | y :: [] -> y
    | y :: _ -> y


(*----------------------------------------------------------------------------*]
 Funkcija [multiply] zmnoži vse elemente seznama. V primeru praznega seznama
 vrne vrednost, ki je smiselna za rekurzijo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # multiply [2; 4; 6];;
 - : int = 48
[*----------------------------------------------------------------------------*)

let rec multiply seznam = 
  match seznam with
  |[] -> 1
  |x :: [] -> x
  |x :: xs -> x * multiply(xs)


(*----------------------------------------------------------------------------*]
 Napišite funkcijo ekvivalentno python kodi:

  def sum_int_pairs(pair_list):
      if len(pair_list) == 0:
        return []
      else:
        x, y = pair_list[0]
        return [x + y] + sum_int_pairs(pair_list[1:])

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_int_pairs [(1, -2); (3, 4); (0, -0)];;
 - : int list = [-1; 7; 0]
[*----------------------------------------------------------------------------*)

let rec sum_int_pairs seznam =
  match seznam with 
  | [] -> []
  | (a, b) :: xs -> a + b :: sum_int_pairs(xs)

(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get k seznam = 
  match (k, seznam) with
  | _, [] -> failwith "prekratko"
  | 0, x :: xs -> x
  | k, x :: xs when k <=  0 -> x
  | k, x :: xs -> get (k-1) xs

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double seznam = 
match seznam with
  | [] -> []
  | x :: xs -> x :: x :: double xs 


(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert n k seznam =
  match k, seznam with
  | _, [] -> [n]
  | k, x :: xs when k <= 0 ->  n :: x :: xs 
  | k, x :: xs -> x :: insert n (k - 1) xs



(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k seznam =
  match k, seznam with
  | k, seznam when k <= 0 -> ([] , seznam)
  | k, []  -> ([] , [])
  | k, x :: xs ->
  let(left_seznam, right_seznam) = divide (k-1) xs in
  (x :: left_seznam , right_seznam)

(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n list =
  let (list1, list2) = divide n list in
  list2 @ list1

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x seznam =
  match x, seznam with
  | _, [] -> []
  | x, y :: ys when x = y -> remove x ys
  | x, y :: ys -> y :: remove x ys

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec obrni seznam = 
  match seznam with
  | [] -> []
  | x :: xs -> (obrni xs) :: x :: []

let rec is_palindrome seznam =
  let obrnjen = obrni seznam in
  seznam = obrnjen

(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components sez1 sez2 =
  match (sez1, sez2) with
  | (x :: xs, y :: ys) -> max x,y :: max_on_components xs ys
  | _ -> []


(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)
let rec najvecje sez =
  match sez with
  |[] -> failwith "napaka"
  |[n] -> n
  |x :: xs -> max x najvecje xs

let rec second_largest seznam =
  match seznam with
  |[] -> failwith "napaka"
  |[x, y] ->  min x, y
  let sez_brez_najvecjih = function
    | [] -> []
    | [a] -> []
    | x :: xs -> if najvecje x :: xs = x then sez_brez_najvecjih xs else x :: sez_brez_najvecjih xs
  in
  najvecje sez_brez_najvecjih seznam

  let second_largest list =
    let rec largest = function
      | [] -> failwith "List is too short."
      | x :: [] -> x
      | x :: xs -> max x (largest xs)
    in
    largest (delete (largest list) list)


(*let rec delete k seznam = 
    match (k, seznam) with
    | _,[] -> failwith "prekratko"
    | 0, x :: xs -> xs
    | k, x :: xs when k <=  0 -> x
    | k, x :: xs -> x :: delete (k-1) xs
    |d*)
    