open Batteries

let test () =
	(1--999)
	|> Enum.filter (fun i -> i mod 3 = 0 || i mod 5 = 0)
        |> Enum.reduce (+)
        |> Int.print stdout


(* Helper functions *)

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
  let a = BatBigarray.Array1.to_array v in
  let vl = Array.fold_left (fun l elem -> elem::l) [] a in
  let cs = List.map f vl in
  make_conjunction ctx cs

let (=*) ctx v k =
  make_constraint ctx (fun x -> Z3.mk_eq ctx x (Z3.mk_int ctx k (Z3.mk_real_sort ctx))) v

let (>=*) ctx v k =
  make_constraint ctx (fun x -> Z3.mk_ge ctx x (Z3.mk_int ctx k (Z3.mk_real_sort ctx))) v

let (<*) ctx v k =
  make_constraint ctx (fun x -> Z3.mk_lt ctx x (Z3.mk_int ctx k (Z3.mk_real_sort ctx))) v


let naturals = BatEnum.unfold 0 (fun i -> Some (i,i))

let vars = BatEnum.map (fun f -> Printf.sprintf "var%d" f) naturals

let fresh_var () = Option.get (BatEnum.get vars)

let solve ctx query =
  let solver = Z3.mk_solver ctx in
  Z3.solver_push ctx solver;
  Z3.solver_assert ctx solver query;
  let ans = Z3.solver_check ctx solver in
(*  Z3.solver_pop ctx solver; *)
  match ans with
  | Z3.L_TRUE -> let model = Z3.solver_get_model ctx solver in Some model
  | Z3.L_FALSE -> None
  | _ -> assert false

let synthesis ctx r =
  let coefs = Matrix.of_array r in
  let r,c = (Matrix.row_dim coefs, Matrix.col_dim coefs) in
  let b = Matrix.sub_left coefs 0 (r-1) (c-1) (c-1) in
  let a = Matrix.sub_left coefs 0 (r-1) 0 ((c-2)/2) in
  let a' = Matrix.sub_left coefs 0 (r-1) (((c-2)/2)+1) (c-2) in
  let b_symb = Matrix.map (make_const ctx) b in
  let a_symb = Matrix.map (make_const ctx) a in
  let a_symb' = Matrix.map (make_const ctx) a' in

  let lambda1 = Array.init r (fun i -> fresh_var ctx ()) in
  let lambda2 = Array.init r (fun i -> fresh_var ctx ()) in

  ()

let ctx = Z3.mk_context []
let slv = Z3.mk_solver ctx

let _ =
  Printf.printf "Hello\n"
