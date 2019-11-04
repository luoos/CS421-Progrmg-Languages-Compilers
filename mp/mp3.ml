let rec subst_fun sigma tv =
    match sigma with
    | [] -> TyVar tv
    | hd::tl ->
        let idx, ty = hd in
            if idx = tv then ty
            else subst_fun tl tv;;

let rec monoTy_lift_subst sigma ty =
    match ty with
    | TyVar (idx) -> subst_fun sigma idx
    | TyConst (str, monoList) ->
        let rec helper li =
            match li with
            | [] -> []
            | hd::tl -> monoTy_lift_subst sigma hd :: helper tl
        in TyConst(str, helper monoList);;

let rec occurs v ty =
    match ty with
    | TyVar (idx) -> v = idx
    | TyConst (str, lis) ->
        let rec helper l =
            match l with
            | [] -> false
            | hd::tl ->
                if (occurs v hd) then true
                else helper tl
        in helper lis;;

(* Bug! *)
let rec unify constraints =
    match constraints with
    | [] -> Some([])
    | (s,t)::rem_constraints ->
        if s = t then unify rem_constraints (* Delete *)
        else
            (match s, t with
            | TyConst _, TyVar _ -> (* Orient *)
                unify ((t, s)::rem_constraints)
            | TyConst (s_name, s_list), TyConst (t_name, t_list) -> (* Decompose *)
                if s_name <> t_name then None
                else if (List.length s_list) != (List.length t_list) then None
                else
                    let rec helper l =
                        match l with
                        | ([], []) -> []
                        | ((shd::stl), (thd::ttl)) ->
                        (shd, thd) :: (helper (stl, ttl))
                    in unify ((helper (s_list, t_list)) @ rem_constraints)
            | TyVar(s_idx), _ -> (* Eliminate *)
                let new_sub = [(s_idx, t)] in
                let rec helper l =
                    match l with
                    | [] -> []
                    | ((l, r) :: tl) ->
                        (monoTy_lift_subst new_sub l, monoTy_lift_subst new_sub r) :: (helper tl)
                in let r = unify (helper rem_constraints)
                in
                    (match r with
                    | None -> None
                    | Some(phi) -> Some((s_idx, monoTy_lift_subst phi t) :: phi))
        );;
