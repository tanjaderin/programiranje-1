(* naloga1*)

let razlika_kvadratov n m =  2 * n * m 

(* 2 naloga *)

let uporabi_na_paru f (a, b) = (f a, f b)

(* 3 naloga*)

let rec ponovi_seznam n sez =
    if n <= 0 then [] else sez :: ponovi_seznam (n-1) sez

(* 4 naloga*)

let razdeli sez =
    let rec razdeli' sez accn accp =
        match sez with
        | [] -> (List.rev accn, List.rev accp)
        | x :: xs when x < 0 -> razdeli' xs (x :: accn) accp
        | x :: xs ->  razdeli' xs  accn (x :: accp)
    in
    razdeli' sez [][]


(* naloga 2
type 'a tree =
    | Empty
    | Node of 'a tree * 'a * 'a tree 

let rec nar_pot  =
    | Empty -> []
    | Node(l, x, d) when l > x & d > x -> if List.legth x :: nar_pot l > List.legth x :: nar_pot d then x :: nar_pot l else nar_pot d
    | Node(l, x, d) when l >  ->  x :: nar_pot l
    | Node(l, x, d) when d > x -> x :: nar_pot d

let rec pad_pot  =
    | Empty -> []
    | Node(l, x, d) when l < x & d < x -> if List.legth x :: nar_pot l > List.legth x :: nar_pot d then x :: nar_pot l else nar_pot d
    | Node(l, x, d) when l < x  ->  x :: pad_pot l
    | Node(l, x, d) when d < x -> x :: pad_pot d

let longer_of lst1 lst2 =
  if List.length lst1 > List.length lst2 then lst1 else lst2

let rec longest_monotone = function
    | Empty -> []
    | Node (l, x, r)->
        let left = longest_monotone l in
        let right = longest_monotone r in

        longer_of

*)

type 'a veriga = 
    | Filter of ('a -> bool) * 'a list * 'a veriga 
    | Ostalo of 'a list

let test =
    (Filter(function x -> (x < 0), []) Filter (function x -> (x < 10), []) Ostalo ([]) )

let rec vstavi x =
    | Filter (f, sez, rep) when f x -> Filter (f , x :: sez, rep )
    | Filter (f ,sez, rep) -> Filter (f, sez, vstavi x rep)
    | Ostalo xy -> Ostalo (x :: xy)

let rec poisci x =
    | Filter (_, y :: ys, rep) when x = y -> true
    | Filter(f, y :: ys, rep) -> poisci Filter (f, ys , rep)