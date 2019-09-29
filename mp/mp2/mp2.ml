(* File: ml3.ml *)

open Common

(* Problem 1 *)
let rec import_list lst =
    match lst with
    | [] -> ConstExp NilConst
    | (a,b)::tl ->
        let int_cons x = ConstExp (IntConst x) in
        BinOpAppExp (ConsOp,
            BinOpAppExp (CommaOp, (int_cons a),
                (int_cons b)), import_list tl);;


(* Problem 2 *)
let pair_sums =
    let var_x = (VarExp "x") and
        var_lst = (VarExp "lst") in
    let get_head = MonOpAppExp (HdOp, var_lst) and
        get_first = MonOpAppExp (FstOp, var_x) and
        get_second = MonOpAppExp (SndOp, var_x) in
    let condition = BinOpAppExp (EqOp, var_lst, (ConstExp NilConst)) in
    let sum = BinOpAppExp (IntPlusOp, (get_first), (get_second)) in
    let rec_call = AppExp ((VarExp "pair_sums"), MonOpAppExp (TlOp, var_lst)) in
    let in_statement = BinOpAppExp (ConsOp, sum, rec_call) in
    let false_result = LetInExp ("x", get_head, in_statement) in
    let exp1 = IfExp (condition, (ConstExp NilConst), false_result) in
    let int_cons x = ConstExp (IntConst x) in
    let input = (BinOpAppExp (ConsOp, BinOpAppExp (CommaOp, (int_cons 7), (int_cons 1)),
            BinOpAppExp (ConsOp, BinOpAppExp (CommaOp, (int_cons 4), (int_cons 2)),
            BinOpAppExp (ConsOp, BinOpAppExp (CommaOp, (int_cons 6), (int_cons 3)),
            ConstExp NilConst)))) in
    let exp2 = AppExp ((VarExp "pair_sums"), input) in
    LetRecInExp ("pair_sums", "lst", exp1, exp2);;


(* Problem 3 *)
let rec count_const_in_exp exp =
    match exp with
    | VarExp x -> 0
    | ConstExp c -> 1
    | MonOpAppExp (_, e1) -> count_const_in_exp e1
    | BinOpAppExp (_, e1, e2) ->
        (count_const_in_exp e1) + (count_const_in_exp e2)
    | IfExp (e1, e2, e3) ->
        (count_const_in_exp e1) +
        (count_const_in_exp e2) +
        (count_const_in_exp e3)
    | AppExp (e1, e2) ->
        (count_const_in_exp e1) + (count_const_in_exp e2)
    | FunExp (_, e) -> count_const_in_exp e
    | LetInExp (_, e1, e2) ->
        (count_const_in_exp e1) +
        (count_const_in_exp e2)
    | LetRecInExp (_, _, e1, e2) ->
        (count_const_in_exp e1) +
        (count_const_in_exp e2);;


(* Problem 4 *)
let rec freeVarsInExp exp =
    match exp with
    | VarExp x -> [x]
    | ConstExp _ -> []
    | MonOpAppExp (_, e) -> freeVarsInExp e
    | BinOpAppExp (_, e1, e2) ->
        (freeVarsInExp e1) @ (freeVarsInExp e2)
    | IfExp (e1, e2, e3) ->
        (freeVarsInExp e1) @ (freeVarsInExp e2) @ (freeVarsInExp e3)
    | AppExp (e1, e2) ->
        (freeVarsInExp e1) @ (freeVarsInExp e2)
    | FunExp (x, e) ->
        (List.filter (fun y -> not (x = y)) (freeVarsInExp e))
    | LetInExp (x, e1, e2) ->
        (freeVarsInExp e1) @ (List.filter (fun y -> not (x = y)) (freeVarsInExp e2))
    | LetRecInExp (f, x, e1, e2) ->
        let r1 = freeVarsInExp e1 and
            r2 = freeVarsInExp e2 in
        let r1_filtered = List.filter (fun y -> not ((f = y) || (x = y))) r1 and
            r2_filtered = List.filter (fun y -> not (f = y)) r2
        in r1_filtered @ r2_filtered;;


(* Problem 5 *)
let rec cps_exp e k =
    match e with
    | VarExp exp -> VarCPS (k, exp)
    | ConstExp exp -> ConstCPS (k, exp)
    | IfExp (e1, e2, e3) ->
        let r2 = cps_exp e2 k and
            r3 = cps_exp e3 k in
        let v2 = freeVarsInExp e2 and
            v3 = freeVarsInExp e3 and
            vf = freeVarsInContCPS k in
        let free_list = v2 @ v3 @ vf in
        let fresh = freshFor free_list
        in cps_exp e1 (FnContCPS (fresh, IfCPS (fresh, r2, r3)))
    | AppExp (e1, e2) ->
        let free_list2 = (freeVarsInExp e1) @ (freeVarsInContCPS k) in
        let v2 = freshFor free_list2 in
        let free_list1 = v2 :: (freeVarsInContCPS k) in
        let v1 = freshFor free_list1
        in cps_exp e2 (FnContCPS (v2, (cps_exp e1 (FnContCPS (v1, (AppCPS (k, v1, v2)))))))
    | BinOpAppExp (op, e1, e2) ->
        let free_list2 = (freeVarsInExp e1) @ (freeVarsInContCPS k) in
        let v2 = freshFor free_list2 in
        let free_list1 = v2 :: (freeVarsInContCPS k) in
        let v1 = freshFor free_list1
        in cps_exp e2 (FnContCPS (v2, cps_exp e1 (FnContCPS (v1, BinOpAppCPS(k, op, v1, v2)))))
    | MonOpAppExp (op, e) ->
        let v = freshFor (freeVarsInContCPS k)
        in cps_exp e (FnContCPS (v, (MonOpAppCPS (k, op, v))))
    | FunExp (x, e) ->
        FunCPS(k, x, Kvar, (cps_exp e (ContVarCPS Kvar)))
    | LetInExp (x, e1, e2) ->
        cps_exp e1 (FnContCPS (x, cps_exp e2 k))
    | LetRecInExp (f, x, e1, e2) ->
        FixCPS (FnContCPS (f, cps_exp e2 k), f, x, Kvar, cps_exp e1 (ContVarCPS Kvar));;
