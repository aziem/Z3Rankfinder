open Batteries
open Matrix

module RankFinder =
  struct
    let make_integer ctx i = Z3.mk_int ctx i (Z3.mk_real_sort ctx)

    let zero ctx = make_integer ctx 0

    let one ctx = make_integer ctx 1

    let add ctx a b = Z3.mk_add ctx [| a; b |]

    let sub ctx a b = Z3.mk_sub ctx [| a; b |]

    let mul ctx a b = Z3.mk_mul ctx [| a ;b |]

    let neg ctx a = mul ctx (make_integer ctx (-1)) a

    let make_var ctx (name:string) =
      let sym = Z3.mk_string_symbol ctx name in
      Z3.mk_const ctx sym (Z3.mk_real_sort ctx)

    let make_const ctx x = make_integer ctx x

    let make_conjunction ctx xs = Z3.mk_and ctx (Array.of_list xs)

    let make_constraint ctx f v =
      let vl = Array.fold_left (fun l elem -> elem::l) [] v in
      let cs = List.map f vl in
      make_conjunction ctx cs

    let print_ast ctx ast =
      Printf.printf "AST: %s\n" (Z3.ast_to_string ctx ast)
    let (=*) ctx v k =
      make_constraint ctx (fun x -> Z3.mk_eq ctx x (Z3.mk_int ctx k (Z3.mk_real_sort ctx))) v

    let (>=*) ctx v k =
      make_constraint ctx (fun x -> Z3.mk_ge ctx x (Z3.mk_int ctx k (Z3.mk_real_sort ctx))) v

    let (<*) ctx v k =
      make_constraint ctx (fun x -> Z3.mk_lt ctx x (Z3.mk_int ctx k (Z3.mk_real_sort ctx))) v

    let solve ctx query =
      let solver = Z3.mk_solver ctx in
      Z3.solver_push ctx solver;
      Z3.solver_assert ctx solver query;
      let ans = Z3.solver_check ctx solver in
      Z3.solver_pop ctx solver 1;
      match ans with
      | Z3.L_TRUE -> let model = Z3.solver_get_model ctx solver in Some model
      | Z3.L_FALSE ->
        None

      | _ -> assert false

    let vector_multiply v m ctx =
      Matrix.mvmul m v (mul ctx) (add ctx) (zero ctx)

    let print_matrix ctx m =
      Array.iter (fun a ->
        Printf.printf "[ ";
        (Array.iter (fun a1 -> Printf.printf "%s ; " (Z3.ast_to_string ctx a1)) a;
         Printf.printf "]\n")) m

    let synthesis r =
      let ctx = Z3.mk_context [] in
      Z3.update_param_value ctx  "MODEL" "true";
      let naturals = BatEnum.from_loop 0 (fun i -> (i+1,i+1)) in

      let vars = BatEnum.map
        (fun f -> make_var ctx (Printf.sprintf "var%d" f)) naturals in

      let fresh_var () =
        Option.get (BatEnum.get (vars)) in

      let coefs = Matrix.of_array r in
      let c,r = (Matrix.row_dim coefs, Matrix.col_dim coefs) in

      let b = Matrix.sub_left coefs 0 (r-1) (c-1) (c-1) in
      let b = Matrix.map (fun i -> (-1) * i) b in
      let a = Matrix.sub_left coefs 0 (r-1) 0 ((c-2)/2) in
      let a' = Matrix.sub_left coefs 0 (r-1) (((c-2)/2)+1) (c-2) in

      let b_symb = Matrix.map (make_const ctx) b in
      let a_symb = Matrix.map (make_const ctx) a in
      let a_symb' = Matrix.map (make_const ctx) a' in

      let lambda1 = Array.init (r) (fun i -> fresh_var ()) in
      let lambda2 = Array.init (r) (fun i -> fresh_var ()) in
      let diff_lambda = Matrix.vminus lambda1 lambda2 (sub ctx) in

      let sum_a = Matrix.add a_symb a_symb' (add ctx) (zero ctx) in

      let c1 = (>=*) ctx lambda1 0 in
      let c2 = (>=*) ctx lambda2 0 in
      let c3 = (=*) ctx (vector_multiply lambda1 (a_symb') ctx ) 0 in
      let c4 = (<*) ctx (vector_multiply lambda2 (b_symb) ctx) 0 in
      let c5 = (=*) ctx (vector_multiply diff_lambda (a_symb) ctx ) 0 in
      let c6 = (=*) ctx (vector_multiply lambda2 (sum_a) ctx ) 0 in
      let cs = [c1;c2;c3;c4;c5;c6] in


      let query = make_conjunction ctx cs in
      match solve ctx query with
      | Some m ->

        let get x = snd (Z3.get_numeral_int ctx (Option.get (Z3.model_eval ctx m x true))) in
        let lambda1_inst = Array.map get lambda1 in
        let lambda2_inst = Array.map get lambda2 in

        let r = Matrix.mvmul a' lambda2_inst (fun i j -> i * j) (fun i j -> i + j) 0 in
        let delta_0 = Matrix.mvmul b lambda1_inst (fun i j -> i * j) (fun i j -> i + j) 0 in
        let delta_0 = Array.map (fun i -> (-1) * i) delta_0 in
                Some (r, delta_0)
      | None -> None

end

(* Test Code *)
let _ =

  let r = [| [| -1;0;0 |]
          ; [| -1;1;1 |]
          ; [| 1;-1;-1|] |] in
  let r2 = [| [| -1;0;1|]
           ; [| -1;1;-1|]
           ; [| 1;-1;1|] |] in

  let r3 = [| [|-1;0;0;0;1|]
           ; [|-1;0;1;0;1|]
           ; [|1;0;-1;0;-1|]
           ; [|0;-1;0;0;1|]
           ; [|0;1;0;-1;1|] |]
  in
  let r4 = [| [|-1;1;0;0;1|]
           ; [|-1;0;1;0;0|]
           ; [|0;1;0;-1;1|] |] in

  let rs = [ r; r2; r3; r4 ] in
  let results = List.map RankFinder.synthesis rs in

  List.iter (fun p ->
    match p with
    | Some (r,d) ->
      Printf.printf "Well founded.\n";
      Printf.printf "row vec: ";
      Array.iter (fun i -> Printf.printf " %d " i) r;
      Printf.printf "\ndelta_0 vec:";
      Array.iter (fun i -> Printf.printf " %d " i) d;
      Printf.printf "\n";

    | None -> Printf.printf "Not well-founded.\n") results
