(* Question 1 *)
let rec product l =
    match l with
    | [] -> 1.0
    | hd :: tl -> hd *. (product tl);;

(* Question 2 *)
let rec double_all l =
    match l with
    | [] -> []
    | hd :: tl -> (2.0 *. hd) :: (double_all tl);;

(* Question 3 *)
let rec pair_with_all x l =
    match l with
    | [] -> []
    | hd :: tl -> (x, hd) :: (pair_with_all x tl);;

(* Question 4 *)
let rec interleave l1 l2 =
    match (l1, l2) with
    | [], [] -> []
    | _, []  -> l1
    | [], _  -> l2
    | (hd1::tl1), (hd2::tl2) -> hd1 :: hd2 :: (interleave tl1 tl2);;

(* Question 5 *)
let rec sub_list l1 l2 =
    match (l1, l2) with
    | [], [] -> true
    | _, [] -> true
    | [], _ -> false
    | (hd1::tl1), (hd2::tl2) -> if hd1 = hd2 then sub_list tl1 tl2
                                else sub_list tl1 l2;;

(* Question 6 *)
let rec even_count_fr l =
    match l with
    | [] -> 0
    | hd::tl -> ((hd + 1) mod 2) + even_count_fr tl;;

(* Question 7 *)
let rec pair_sums l =
    match l with
    | [] -> []
    | (x,y)::tl -> (x + y) :: pair_sums tl;;

(* Question 8 *)
let rec remove_even list =
    match list with
    | [] -> []
    | hd::tl -> if (hd mod 2) = 0 then remove_even tl
                else hd :: remove_even tl;;

(* Question 9 *)
let rec sift p l =
    match l with
    | [] -> ([],[])
    | hd::tl -> let (true_el, false_el) = sift p tl in
                if (p hd) then (hd :: true_el, false_el)
                else (true_el, hd :: false_el);;

(* Question 10 *)
let rec apply_even_odd l f g =
    match l with
    | [] -> []
    | hd::tl -> (f hd) :: apply_even_odd tl g f;;

(* Question 11 *)
let rec even_count_tr l =
    let rec helper l sum =
        match l with
        | [] -> sum
        | hd::tl -> helper tl (sum + ((hd + 1) mod 2))
    in helper l 0;;

(* Question 12 *)
let rec count_element l m =
    let rec helper l m cnt = 
        match l with
        | [] -> cnt
        | hd::tl -> if hd = m then helper tl m (cnt+1)
                    else helper tl m cnt
    in helper l m 0;;

(* Question 13 *)
let rec all_nonneg list =
    match list with
    | [] -> true
    | hd::tl -> if hd >= 0 then all_nonneg tl else false;;

(* Question 14 *)
let rec split_sum l f =
    let rec helper l f res =
        match l with
        | [] -> res
        | hd::tl -> let (s1, s2) = res in
                    if f hd then helper tl f (s1+hd, s2)
                    else helper tl f (s1, s2+hd)
    in helper l f (0, 0);;

(* Question 15 *)
let rec concat s list =
    let rec helper s list res =
        match list with
        | [] -> res
        | [x] -> res ^ x
        | hd::tl -> helper s tl (res ^ hd ^ " " ^ s ^ " ")
    in helper s list "";;

(* Question 16 *)
let even_count_fr_base = 0;;
let even_count_fr_rec x rec_val =
    if (x mod 2) == 0 then 1 + rec_val else rec_val;;

(* Question 17 *)
let pair_sums_map_arg p =
    let (x, y) = p in x + y;;

(* Question 18 *)
let remove_even_base = [];;
let remove_even_rec n r = if (n mod 2) = 0 then r else n::r;;

(* Question 19 *)
let even_count_tr_start = 0;;
let even_count_tr_step acc_val x =
    if (x mod 2) = 0 then acc_val + 1
    else acc_val;;

(* Question 20 *)
let split_sum_start = (0, 0);;
let split_sum_step f p i =
    let (x, y) = p in
    if f i then (x+i, y) else (x, y+i);;
