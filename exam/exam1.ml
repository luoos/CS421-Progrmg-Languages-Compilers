(* Question 1 *)
let rec delannoy (m, n) =
    match (m, n) with
    | (0, 0) -> 1
    | (0, _) -> 1
    | (_, 0) -> 1
    | (m, n) -> delannoy (m-1, n) + delannoy(m, n-1) + delannoy(m-1, n-1);;

let swap_eq p1 p2 =
    let (x1, y1) = p1 and
        (x2, y2) = p2 in
    x1 = y2 && y1 = x2;;

(* Question 2 *)
let rec upto n =
    if n < 0 then []
    else if n = 0 then [0]
    else upto (n-1) @ [n];;

(* Question 8 *)
let exists_between_start = false;;
let exists_between_step m n b x =
    if m <= x && x <= n then true || b
    else false || b;;

let rec split_sum l f =
    let rec helper l f (s1, s2) =
        match l with
        | [] -> (s1, s2)
        | hd::tl -> if f hd then helper tl f (s1 + hd, s2)
                    else helper tl f (s1, s2 + hd)
    in helper l f (0, 0);;

let split_sum_start = (0,0)

let split_sum_step f =
    fun (s1, s2) ->
        fun i -> if f i then (s1+i, s2) else (s1, s2+i);;
