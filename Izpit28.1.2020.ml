(* 1 naloga *)

(*let option_sum  c d =
    if Some (Option.value c + Option.value d ) else None *)

let twostep_map f1 f2 f3 a = 
    let (b, c) = f1(a)
    in
    (f2 b, f3 c ) 

let function_repeat f xs = 
    let rec repeat' acc i f xs =
    match xs with
    | [] -> List.rev acc
    | x :: xs' when i = 0 -> repeat' (x :: acc) (f x) f xs
    | x :: xs' when i = 1 -> repeat' (x :: acc) 0 f xs'
    | x :: xs' -> repeat' (x :: acc) (i - 1) f xs'
    in
    repeat' [] 0 f xs

let rec iterate f pogoj x =
    if pogoj (f x) = true then f x else iterate f pogoj (f x)



(* 2 naloga *)

type improved_list = 
    | Empty
    | Node of int array * improved_list

let test = Node([|1;2;20|], Node( [|17;19;20;30|], Node( [|100|], Empty)))

let rec count = function
    | Empty -> 0
    | Node ( xs , preostanek) -> Array.length xs + count preostanek

let rec nth x sez = 
    match sez with
    | Empty -> None
    | Node ( xs , preostanek) -> if (Array.length xs - 1) <= x then Some (xs.(x)) else nth (x - (Array.length xs) + 1) preostanek



let swap a i j =
    let z = a.(i) in
    a.(i) <- a.(j); 
    a.(j) <- z

let index_min a lower upper=
     let cur_ind = ref lower in
     for i = (lower + 1) to upper
     do
        if a.(i ) < a.(!cur_ind)
        then cur_ind := i
    done;
    !cur_ind

let sort_array a =
    let index_end = Array.length a - 1 in
    for boundary_sorted = 0 to index_end do
      let i = index_min a boundary_sorted index_end in
      swap a i boundary_sorted
    done;
    a
    
let rec is_sorted = function
    | Empty -> true
    | Node ( xs , preostanek) -> if xs = sort_array xs then is_sorted preostanek else false

let zamenjaj a i x = (a.(i) <- x); a

let rec update sez i x = 
    match sez with
    | Empty -> [||]
    | Node ( xs, preostanek) -> if ( Array.length xs - 1 ) <= i then zamenjaj xs i x else update preostanek (i - Array.length xs + 1) x


