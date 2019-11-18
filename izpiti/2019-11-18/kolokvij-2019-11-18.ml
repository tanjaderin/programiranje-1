(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root x y = if x > 0 && x * x = y then true else false

let pack3 x y z = (x, y, z)

let sum_if_not f seznam =
    let rec sum' acc f seznam = 
    match seznam with
    | [] -> acc
    | x :: xs when f x = false -> sum' (x + acc) f xs
    | x :: xs when f x = true-> sum' acc f xs
    in
    sum' 0 f seznam
 
let apply funkcije seznam =
    let apply' acc funckije seznam =
    match (funkcije, seznam) with
    | ([],[]) -> acc
    | (f :: fs, []) -> apply' (acc :: acc) fs seznam
    | (f :: fs, x :: xs) -> apply' (fx :: acc) funckije xs

    (* vsakic ceu akumulator prilepi zraven namesto samo 'novejši del' *))

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja =
    | Vaje
    | Predavanja

type srecanje = { predmet = string; vrsta = vrsta_srecanja; trajanje = int}

type urnik =
    | Prazno
    | Srecanje of string * vrsta_srecanja * int

let vaje = Srecanje('Analiza 2a' Vaje 3)
let predavanje = Srecanje('Programiranje 1' Predavanje 2)

let urnik_profesor = Srecanje('Analiza 2a' Vaje 2, Prazno, Srecanje('Programiranje 1' Predavanje 1, Prazno, Prazno , Srecanje('Programiranje 1' Vaje 1)))

let je_preobremenjen urnik = 
    let rec pomozna accv accp urnik=
    match urnik with
    | [] -> true
    | {_; vrsta; stevilo } :: xs when vrsta = Vaje-> if accv <= 4 && accp <= 4 then pomozna (accv + 1) accp xs else false
    | {_; vrsta; stevilo } :: xs when vrsta = Predavanja -> if accv <= 4 && accp <= 4 then pomozna accv (accp + 1) xs else false
    in
    pomozna 0 0 urnik

let bogastvo urnik = 
    let rec izracunaj acc urnik = function
    | [] -> acc
    | {_; vrsta; stevilo } -> 
            let cena = 
            match vrsta with
            | Vaje -> 1
            | Predavanja -> 2
            in stevilo * cena 
    in
    izracunaj 0 urnik 
    (*nekoncano *))


