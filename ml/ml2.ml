(* Question 1 *)
let closer_to_origin p1 p2 =
    let (x1, y1) = p1
    and (x2, y2) = p2 in
    let d1 = x1 *. x1 +. y1 *. y1
    and d2 = x2 *. x2 +. y2 *. y2 in
    if d1 < d2 then -1
    else if d1 > d2 then 1
    else 0;;

(* Question 2 *)
let swap_eq p1 p2 =
    let (x1, y1) = p1
    and (x2, y2) = p2 in
    if x1 = y2 && x2 = y1 then true
    else false;;

(* Question 3 *)
let triple_pairs x trp =
    let (a, b, c) = trp in
    ((x, a), (x, b), (x, c));;

(* Question 4 TODO: incorrect *)
let triple_xprod trp pr =
    let (t1, t2, t3) = trp
    and (p1, p2) = pr in
    ((t1, p1), (t2, p1), (t3, p1),
     (t1, p2), (t2, p2), (t3, p2));;

(* Question 5 *)
let twist pp =
    let ((a, b), (c, d)) = pp in
    ((d, a), (c, b));;

(* Question 6 *)
let map_triple f (a, b, c) =
    ((f a), (f b), (f c));;

(* Question 7 *)
let rev_app x f =
    f x;;

(* Question 8 *)
let two_funs fns ins =
    let (f1, f2) = fns
    and (arg1, arg2) = ins in
    ((f1 arg1), (f2 arg2));;

(* Question 9 *)
let same_arg_twice f x =
    f x x;;

(* Question 10 *)
let triple_app (f,g,h) x =
    f (g (h x));;

(* Question 11 *)
let rec naive_fibonacci n =
    if n < 2 then 1
    else naive_fibonacci (n - 1) + naive_fibonacci (n - 2);;

(* Question 12 *)
let rec collatz n =
    if n = 1 then 0
    else if (n mod 2) = 0 then 1 + collatz (n / 2)
    else 1 + collatz (n * 3 + 1);;

(* Question 13 *)
let rec ackermann m n =
    if m = 0 then n + 1
    else if m > 0 && n = 0 then ackermann (m-1) 1
    else ackermann (m-1) (ackermann m (n-1));;

(* Question 14 *)
let rec sum_evens_less_eq n =
    if n <= 0 then 0
    else if (n mod 2) = 0 then n + sum_evens_less_eq (n-1)
    else sum_evens_less_eq (n-1);;

(* Question 15 *)
let rec delannoy (m, n) =
    if m = 0 && n = 0 then 1
    else
        let x = if m > 0 then delannoy (m-1, n) else 0
        and y = if n > 0 then delannoy (m, n-1) else 0
        and d = if m > 0 && n > 0 then delannoy (m-1, n-1) else 0
        in x + y + d;;

