
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [square] vrne kvadrat podanega celega števila.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # square 2;;
 - : int = 4
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD:4-uvod-v-ocaml/vaje/uvod.ml
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

=======
let rec square = ()
>>>>>>> f5ad26abe35573e9990e8a86b82854540bd76714:04-uvod-v-ocaml/vaje/uvod.ml

(*----------------------------------------------------------------------------*]
 Funkcija [middle_of_triple] vrne srednji element trojice.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # middle_of_triple (true, false, true);;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec middle_of_triple = ()

(*----------------------------------------------------------------------------*]
 Funkcija [starting_element] vrne prvi element danega seznama. V primeru
 prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # starting_element [1; 2; 3; 4];;
 - : int = 1
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD:4-uvod-v-ocaml/vaje/uvod.ml
let rec get k seznam = 
  match (k, seznam) with
  | _,[] -> failwith "prekratko"
  | 0, x :: xs -> x
  | k, x :: xs when k <=  0 -> x
  | k, x :: xs -> get (k-1) xs

let rec get k  = function 
    | _,[] -> failwith "prekratko"
    | 0, x :: xs -> x
    | k, x :: xs when k <=  0 -> x
    | k, x :: xs -> get (k-1) xs
  
=======
let rec starting_element = ()
>>>>>>> f5ad26abe35573e9990e8a86b82854540bd76714:04-uvod-v-ocaml/vaje/uvod.ml

(*----------------------------------------------------------------------------*]
 Funkcija [multiply] zmnoži vse elemente seznama. V primeru praznega seznama
 vrne vrednost, ki je smiselna za rekurzijo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # multiply [2; 4; 6];;
 - : int = 48
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD:4-uvod-v-ocaml/vaje/uvod.ml
let rec double seznam = 
match seznam with
  | [] -> []
  | x :: xs -> x :: x :: double xs 
=======
let rec multiply = ()
>>>>>>> f5ad26abe35573e9990e8a86b82854540bd76714:04-uvod-v-ocaml/vaje/uvod.ml

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

<<<<<<< HEAD:4-uvod-v-ocaml/vaje/uvod.ml
let rec divide k seznam=
  match k, seznam with
  | k, list when (k <= 0) -> ([] , seznam)
  | k, []  -> ([] , [])
  | k, x :: xs ->
  let(left_seznam, right_seznam) = divide (k-1) xs in
  (x :: left_seznam ,right_seznam)

  let rec divide k seznam=
    match k, seznam with
    | k, list when (k <= 0) -> ([] , seznam)
    | k, []  -> ([] , [])
    | k, x :: xs ->
    let(left_seznam, right_seznam) = divide (k-1) xs in
    (x :: left_seznam ,right_seznam)
=======
let rec sum_int_pairs = ()

>>>>>>> f5ad26abe35573e9990e8a86b82854540bd76714:04-uvod-v-ocaml/vaje/uvod.ml
(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD:4-uvod-v-ocaml/vaje/uvod.ml
let rec delete k seznam = 
    match (k, seznam) with
    | _,[] -> failwith "prekratko"
    | 0, x :: xs -> xs
    | k, x :: xs when k <=  0 -> x
    | k, x :: xs -> x :: delete (k-1) xs
    |d
    
=======
let rec get = ()
>>>>>>> f5ad26abe35573e9990e8a86b82854540bd76714:04-uvod-v-ocaml/vaje/uvod.ml

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double = ()

(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert = ()

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

let rec divide = ()

(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate = ()

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove = ()

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_palindrome = ()

(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components = ()

(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let rec second_largest = ()
