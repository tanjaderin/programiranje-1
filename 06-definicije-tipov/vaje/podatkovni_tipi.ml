(* ========== Vaja 3: Definicije Tipov  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)

(*let x num num =
type int_option = None | Some of int
type a option = None | Some of a
*)

(*type barva = 
|red
|blue
|yellow
|rgb of int * int * int  *)

(*type a list = 
|Empty (* []*)
|Cons  of a * a list (* x:: xs *)
 *)

 (*type a drevo = 
|Empty (* []*)
|node  of a * a drevo * a drevo (* x:: xs *)
 
let has_zero tree=
  match tree with
  |emty->
  |node(x, left_tree,right_tree)->

*)


type euro  = Euro of float
type dollar = Dollar of float

let dollar_to_euro_fake dollar = 0.2 *. dollar
let dollar_to_euro dollar = 
  match dollar with 
  |Dollar v ->Euro(0.2*. v)

let euro_to_dollar euro =
  match euro with
  | Euro e -> Dollar(e *. 1.2 )

(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en vsotni tip z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)

type currency =
  |Yen of float
  |Pound of float
  |Krona of float
  |Franki of float

let to_pound currency =
  match currency with
   |Yen y -> Pound (1. *. y)
   |Pound y -> Pound y
   |Krona y -> Pound(0.3 *.y)
   |Franki y -> Pound(1.2 *. y)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
(*)
type celo_ali_znak =
  |Logicna of bool
  |Celo of int

type 'celo_ali_znak seznam =
  | Prazen
  | Sestavljen of 'celo_ali_znak * 'celo_ali_znak seznam *) 

(*----------------------------------------------------------------------------*]
 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

type intbool_list =
  | Empty
  | Int of int * intbool_list
  | Bool of bool * intbool_list

let Int(5, Bool(true,(Bool(false Intval 7))))
(*----------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)
(*let rec intbool_map(f_int : int -> int)(f_bool : bool->bool) = function*)
(*
let rec map f = function
|[] ->  []
|x:: xs -> f x :: map xs*)

let rec intbool_map (f_int : int -> int)(f_bool : bool -> bool) = function
| Empty -> Empty
| Int (i, ib_list) -> Int (f_int i , intbool_map f_int f_bool ib_list)
| Bool (b , ib_list) -> Bool (f_bool b , intbool_map f_int f_bool ib_list)


let rec intbool_map(f_int : int -> int)(f_bool : bool -> bool) = function
| Empty -> Empty
| Intval (i, ib_list) ->
let new_i = f_int i in
let new_tail = intbool_map f_int f_bool ib_list in
Intval(new_i, new_tail)
|Intbool (b , ib_list) ->
let new_b = f_bool b in
let new_tail = intbool_map f_int f_bool ib_list in
Boolval (new_b, new_tail)


(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse ib_list =
  let rec reverse' (acc : intbool_list) = function
   |Empty-> acc
   |Int (i, ib_tail )-> reverse' Int(i, acc) ib_tail
   |Bool(b, ib_tail)-> reverse' Boolval(b,acc) ib_tail
  in reverse' Empty ib_list

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let rec intbool_separate ib_sez = 
  let rec loci' (acc1: intbool_list) (acc2: intbool_list) ib_sez =
    match ib_sez with
    | Empty -> (acc1, acc2)
    | Int (i, rep) -> loci' (Int(i, acc1)) acc2 rep
    | Bool(b. rep) -> loci' acc1 (Boll(b, acc2)) rep
  in
  loci' Empty Empty (intbool_reverse ib_sez)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)
 type magic =
  |Fire
  |Frost
  |Arcane

type specialisation =
  |Historian
  |Teacher
  |Researcher
(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)
type status =
  | Newbie
  | Student of magic * int
  | Employed of magic * specialisation

  type wizard = { name = string; status = status}

  let professor = {name = "Matija"; status = Employed(Fire, Teacher)}
(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)
type magic_counter =  { fire; int, frost: int, arcane: int}

let update stevec = function
  |Fire -> {stevec with fire = stevec.fire + 1 }
  |Frost -> {stevec with frost = stevec.frost + 1 }
  |Arcane -> {stevec with arcane = stevec.arcane + 1 }



(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)
let count_magic wizard_list =
  let rec count counter = function
    | [] -> counter
    | {name; status} :: wizards -> (
        match status with
        | Newbie -> count counter wizards
        | Student (magic, _) -> count (update counter magic) wizards
        | Employed (magic, _) -> count (update counter magic) wizards)
  in count {fire = 0; frost = 0; arcane = 0} wizard_list

(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let find_candidate magic specialisation wizard_list =
  let year =
    match specialisation with
    | Historian -> 3
    | Researcher -> 4
    | Teacher -> 5
  in
  let rec search = function
    | [] -> None
    | {name; status} :: wizards ->
        match status with
        | Student (m, y) when m = magic && y >= year -> Some name
        | _ -> search wizards
  in
  search wizard_list