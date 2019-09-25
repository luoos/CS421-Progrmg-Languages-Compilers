(* Question 1 *)
let consk (x, l) k =
    k (x::l);;

let concatk (s1, s2) k =
    k (s1^s2);;

let string_of_intk n k =
    k (string_of_int n);;

let truncatek x k =
    k (truncate x);;

(* Question 2 *)
let diff_flipk p k =
    subk (1, p)
        (fun a -> mulk (a, p)
        (fun b -> mulk (2, b) k));;

let shiftk (s, q) k =
    float_addk (q, 1.57)
        (fun a -> float_mulk (a, a)
        (fun b -> truncatek b
        (fun c -> string_of_intk c
        (fun d -> concatk (s, d)
        (fun e -> concatk (e, s) k)))));;

(* Take care of the evaluation order *)
let quadk (a, b, c) k =
    mulk (4, b)
        (fun r -> mulk (a, a)
        (fun l -> mulk (2, l)
        (fun m -> addk (m, r)
        (fun n -> addk (n, c) k))));;


(* Question 3 *)

(* Only use forward recursion *)
let rec even_count l =
    match l with
    | [] -> 0
    | x::xs -> let r = even_count xs in
               let m = (x mod 2) in
               if m = 0 then 1 + r
               else r;;

let rec even_countk l k =
    match l with
    | [] -> k 0
    | x::xs -> 
        even_countk xs
            (fun r -> modk (x, 2)
            (fun a -> eqk (a, 0)
            (fun b -> if b then addk (1, r) k
                      else k r)));;

let rec all_positive l =
    match l with
    | [] -> true
    | x::xs -> if x >= 0 then all_positive xs
               else false;;

let rec all_positivek l k =
    match l with
    | [] -> k true
    | x::xs ->
        geqk (x, 0)
            (fun a -> 
                if a then all_positivek xs k
                else k false);;

let rec list_prod l =
    match l with
    | [] -> 1
    | x::xs -> x * list_prod xs;;

let rec list_prodk l k =
    match l with
    | [] -> k 1
    | x::xs -> list_prodk xs (fun r -> mulk (x, r) k);;

(* Question 4 *)
let rec find_all (p, l) =
    match l with
    | [] -> []
    | x::xs -> if p x then x :: find_all (p, xs)
               else find_all (p, xs);;

let rec find_allk (p, l) k =
    match l with
    | [] -> k []
    | x::xs ->
        p x 
            (fun a ->
                if a then find_allk (p, xs) (fun b -> consk (x, b) k)
                else find_allk (p, xs) k);;
    

(* Question 5 *)
let rec list_compose fs =
    match fs with
    | [] -> 0
    | [f] -> f 0
    | f::fs -> f (list_compose fs);;

let rec list_composek fsk k =
    match fsk with
    | [] -> k 0
    | [f] -> f 0 k
    | f::fs -> list_composek fs (fun r -> f r k);;
