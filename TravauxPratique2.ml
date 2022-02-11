(* renvoie n nombres aléatoires *)
let rec lalea n l =
	if 0 = n then l
	else lalea (n-1)
	((Random.int 1000)::l);;

(* Fusion de deux listes triées: *)
let rec merge cmp a b =
    match a with
    | [] -> b
    | ta::qa -> (
        match b with
        | [] -> a
        | tb::qb -> if cmp ta tb <= 0
                        then ta::(merge cmp qa b)
                        else tb::(merge cmp a qb)
    );;
merge (-) [1;4;10;20] [2;3;5;11;30;40];;

(* Couper une liste en 2 *)
let rec moitie l = 
    match l with
    | [] | [_] -> l
    | t1::t2::q-> t1::(moitie q);;

moitie [1;2;3;4;5];;

(* Trier les valeurs: merge sort *)
let rec mergesort cmp l = 
    match l with
    | [] | [_] -> l
    | _::q -> merge cmp (mergesort cmp (moitie l)) (mergesort cmp (moitie q));;


mergesort (-) (lalea 100 [])

(* Obtenir le minimum d'une liste *)
let rec min w =
    match w with
    |[] -> failwith "Erreur"
    |[x] -> x
    |t::q -> let m = min q in
        if t < m
            then t
            else m;;

(* Obtenir le maximul d'une liste *)
let rec max w =
    match w with
    |[] -> failwith "Erreur"
    |[x] -> x
    |t::q -> let m = min q in
        if t > m
            then t
            else m;;

min [43;23;432;234];;

(* Retourner la somme des n premiers entiers *)
let rec why n = 
    match n with
    | 0 -> 0
    | _ -> n + why (n-1);;

why 12;;

type nTree = L of int * nTree list;;


let rec sumt arb =
  match arb with
  |L(x,[]) -> x
  |L(x,t::q) ->
      let s1 = sumt t in
      let s2 = sumt (L(0,q)) in
      s1 + s2 + x;; 


let q = L(5,[ L(7,[L(0,[]);L(0,[])]); L(3,[L(0,[])]); L(0,[]) ]);;

(* Appliquer une fonction à chaque élément d'une liste*)
let rec map f l =
	match l with
		| [] -> []
		| t::q -> (f t)::(map f q);;


(* Renvoie toutes les partitions d'une liste *)
let rec subsets l =
	match l with
	| [] -> [[]]
	| t::q -> 
		let partsq = subsets q in
		partsq@(map (function s -> t::s) partsq);;

subsets [1;2;3];;

(* Filtrer une liste par un prédicat *)
let rec filtrer pred l = 
	match l with
		| [] -> []
		| t::q -> if pred t
			then t::(filtrer pred q)
			else filtrer pred q;;

(* Liste de i jusqu'à j *)
let rec iaj i j = 
	if i > j then []
	else i::(iaj(i+1)j);;

(* Problème: une reine par ligne, une par colonne, une par diagonale *)
let ta = [| 1;2;3;4|];;

(* Structure modélisant un échiquier *)
type echi = {
    npos: int;
    lig: int array
};;

(* Si on peut poser une reine en l *)
let rec possible echi l = 
    let n = Array.length echi.lig in
    let lig = echi.lig in
    let np = echi.npos in
    let rec conflit c = 
        c < np (* np, pas n BUG!!! *) 
        && (lig.(c) = l || abs(l - lig.(c)) = abs(np-c)
            || conflit(c+1)) in
            not (conflit 0);;

let poser echi li =
    let n = Array.length echi.lig in
    let co = echi.npos in
    let li = Array.init n
    (function c -> if c = co
                    then li
                    else echi.lig.(c)) in
    {
        npos=1+co;
        lig=li
    };;

let rec fils ech = 
    let n = Array.length ech.lig in
    if ech.npos = n then []
    else map (function l -> poser ech l) (filtrer (possible ech) (iaj 0 (n-1)));;


let rec search solp fiston pile sols = 
    match pile with 
    | [] -> sols
    | t::q -> if solp t
        then search solp fiston q (t::sols)
        else search solp fiston ((fiston t)@q) sols;;

let reines n =
    let echi = {
        npos=0;
        lig=Array.init n (function l -> -1) (* Array au lieu de A *)
    } in
    map (function ec->ec.lig)
        (search (function ec -> ec.npos=n) fils [echi] []);;

reines 4;;
reines 8;;
