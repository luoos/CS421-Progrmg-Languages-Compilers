let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
    match exp
    with ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma))
      | VarExp x ->
        let x_type = lookup_env gamma x in
        (match x_type with
            | None -> None
            | Some tx ->
                let unified = unify [(tau, freshInstance tx)] in
                (match unified with
                    | None -> None
                    | Some sub ->
                        Some(Proof([], judgment), sub)
                )
        )
      | MonOpAppExp (monop, e1) ->
        let tau1 = fresh() in
        let e1_result = gather_exp_ty_substitution gamma e1 tau1 in
        (match e1_result with
            | None -> None
            | Some(e1_proof, sigma) ->
                let fun_ty_1 = mk_fun_ty tau1 tau in
                let tau' = monop_signature monop in
                let monoed_ty = monoTy_lift_subst sigma fun_ty_1 in
                let unified = unify [(monoed_ty, freshInstance tau')] in
                ( match unified with
                    | None -> None
                    | Some sub ->
                        Some(Proof([e1_proof], judgment), subst_compose sub sigma)
                )
        )
      | IfExp (e1, e2, e3) ->
        let e1_result = gather_exp_ty_substitution gamma e1 bool_ty in
        ( match e1_result with
            | None -> None
            | Some(e1_proof, sigma1) ->
                let gamma_2 = env_lift_subst sigma1 gamma and
                    tau2 = monoTy_lift_subst sigma1 tau in
                let e2_result = gather_exp_ty_substitution gamma_2 e2 tau2 in
                ( match e2_result with
                    | None -> None
                    | Some(e2_proof, sigma2) ->
                        let composed_s1_s2 = subst_compose sigma2 sigma1 in
                        let gamma_3 = env_lift_subst composed_s1_s2 gamma in
                        let tau3 = monoTy_lift_subst composed_s1_s2 tau in
                        let e3_result = gather_exp_ty_substitution gamma_3 e3 tau3 in
                        ( match e3_result with
                            | None -> None
                            | Some(e3_proof, sigma3) ->
                                Some(Proof([e1_proof;e2_proof;e3_proof], judgment), subst_compose sigma3 composed_s1_s2)
                        )
                )
        )
      | FunExp (x, e) ->
        let tau1 = fresh() in
        let gamma_1 = ins_env gamma x (polyTy_of_monoTy tau1) in
        let tau2 = fresh() in
        let e1_result = gather_exp_ty_substitution gamma_1 e tau2 in
        ( match e1_result with
            | None -> None
            | Some(e1_proof, sigma) ->
                let fun_ty_1_2 = mk_fun_ty tau1 tau2 in
                let monoed_ty_1_2 = monoTy_lift_subst sigma fun_ty_1_2 in
                let monoed_ty = monoTy_lift_subst sigma tau in
                let unified = unify [(monoed_ty, monoed_ty_1_2)] in
                ( match unified with
                    | None -> None
                    | Some sub ->
                        Some(Proof([e1_proof], judgment), subst_compose sub sigma)
                )
        )
      | AppExp (e1, e2) ->
        let tau1 = fresh() in
        let e1_result = gather_exp_ty_substitution gamma e1 (mk_fun_ty tau1 tau) in
        ( match e1_result with
            | None -> None
            | Some(e1_proof, sigma1) ->
                let gamma_2 = env_lift_subst sigma1 gamma in
                let e2_result = gather_exp_ty_substitution gamma_2 e2 (monoTy_lift_subst sigma1 tau1) in
                ( match e2_result with
                    | None -> None
                    | Some(e2_proof, sigma2) ->
                        Some(Proof([e1_proof;e2_proof], judgment), subst_compose sigma2 sigma1)
                )
        )
      | RaiseExp e ->
        let e_result = gather_exp_ty_substitution gamma e int_ty in
        (match e_result with
            | None -> None
            | Some(proof, sigma)
                -> Some(Proof([proof], judgment), sigma)
        )
      | LetInExp (x, e1, e2) ->
        let tau1 = fresh() in
        let e1_result = gather_exp_ty_substitution gamma e1 tau1 in
        (match e1_result with
            | None -> None
            | Some(e1_proof, sigma1) ->
                let gamma_2' = env_lift_subst sigma1 gamma in
                let tau_1_lift = monoTy_lift_subst sigma1 tau1 in
                let genned = gen gamma_2' tau_1_lift in
                let gamma_2 = ins_env gamma_2' x genned in
                let tau_2 = monoTy_lift_subst sigma1 tau in
                let e2_result = gather_exp_ty_substitution gamma_2 e2 tau_2 in
                (match e2_result with
                    | None -> None
                    | Some(e2_proof, sigma2) ->
                        Some(Proof([e1_proof;e2_proof], judgment), (subst_compose sigma2 sigma1))
                )
        )
      | LetRecInExp (f, x, e1, e2) ->
        let tau1 = fresh() and
            tau2 = fresh() in
        let gamma_1' = ins_env gamma f (polyTy_of_monoTy (mk_fun_ty tau1 tau2)) in
        let gamma_1 = ins_env gamma_1' x (polyTy_of_monoTy tau1) in
        let e1_result = gather_exp_ty_substitution gamma_1 e1 tau2 in
        ( match e1_result with
            | None -> None
            | Some(e1_proof, sigma1) ->
                let gamma_2' = env_lift_subst sigma1 gamma in
                let f_type = gen (gamma_2') (monoTy_lift_subst sigma1 (mk_fun_ty tau1 tau2)) in
                let gamma_2 = ins_env gamma_2' f f_type in
                let e2_result = gather_exp_ty_substitution gamma_2 e2 (monoTy_lift_subst sigma1 tau) in
                ( match e2_result with
                    | None -> None
                    | Some(e2_proof, sigma2) ->
                        Some(Proof([e1_proof;e2_proof], judgment), subst_compose sigma2 sigma1)
                )
        )
      |  BinOpAppExp (binop, e1,e2) ->
        let tau' = binop_signature binop and
            tau1 = fresh() and
            tau2 = fresh() in
        let e1_result = gather_exp_ty_substitution gamma e1 tau1 in
            ( match e1_result with
                | None -> None
                | Some(e1_proof, sigma1) ->
                let gamma_2 = env_lift_subst sigma1 gamma in
                let e2_result = gather_exp_ty_substitution gamma_2 e2 tau2 in
                ( match e2_result with
                    | None -> None
                    | Some(e2_proof, sigma2) ->
                        let fun_ty = mk_fun_ty tau1 (mk_fun_ty tau2 tau) in
                        let composed = subst_compose sigma2 sigma1 in
                        let monoed = monoTy_lift_subst composed fun_ty in
                        let unified = unify [(monoed, freshInstance tau')] in
                        ( match unified with
                            | None -> None
                            | Some sub ->
                                Some(Proof([e1_proof;e2_proof], judgment), subst_compose sub composed)
                        )
                )
            )