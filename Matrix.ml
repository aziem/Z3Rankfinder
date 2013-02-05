open Printf

type 'a matrix = 'a array array

exception Not_a_matrix
exception Not_same_dimension
exception Empty_matrix
exception Wrong_dimension


module type MATRIX_TYPE =
  sig
    type t
    val plus : t -> t -> t
    val sub : t -> t -> t
    val mult : t -> t -> t
    val init : t
  end


module Matrix (M : MATRIX_TYPE) =
  struct
  let of_array = function
    | [|[||]|] -> [|[||]|]
    | m ->
      let col_dim = Array.length m in
      let row_dim = Array.length m.(0) in
      for i=1 to col_dim - 1 do
	if row_dim <> (Array.length m.(i)) then raise Not_a_matrix
      done;
      m

  let to_array a = a

  let copy a =
    Array.map (Array.copy) a

  let col_dim m =
    Array.length m

  let row_dim m =
    Array.length m.(0)

  let unsafe_set m i j x =
    Array.unsafe_set (Array.get m i) j x

  let get m i j =
    m.(i).(j)

  let unsafe_get m i j =
    Array.unsafe_get (Array.unsafe_get m i) j

  let create col_dim row_dim x =
    let res = Array.create row_dim [||] in
    for i=0 to row_dim - 1 do
      Array.unsafe_set res i (Array.create col_dim x)
    done;
    res

  let init col_dim row_dim f =
    let res = Array.create row_dim [||] in
    for i=0 to row_dim - 1 do
      Array.unsafe_set res i (Array.create col_dim (f i 0));
      for j=1 to col_dim - 1 do
	Array.unsafe_set (Array.unsafe_get res i) j (f i j)
      done;
    done;
    res
  let transpose m =
    init (col_dim m) (row_dim m) (fun i j -> unsafe_get m j i)


  let multiply x y  =
    let x0 = Array.length x in
    let y0 = Array.length y in
    let y1 = if y0=0 then 0 else Array.length y.(0) in
    let z = create x0 y0 M.init in
    for i = 0 to x0-1 do
      for j = 0 to y1-1 do
        for k = 0 to y0-1 do
          z.(i).(j) <- M.plus z.(i).(j) (M.mult  x.(i).(k) y.(k).(j))
        done
      done
    done;
    z

  let scalar_multiply i m =
    let r = Array.length x in
    let c = Array.length x.(0) in
    let z = create r c M.init in
    for i = 0 to r do
      for j = 0 to c do
        z.(i).(j) <- M.mult i x.(i).(j);
      done;
    done;
    z


  let add m1 m2  =
    Array.mapi (fun i r -> (Array.mapi (fun j c -> M.plus m1.(i).(j) m2.(i).(j)) r) ) m1

  let vminus v1 v2 =
    Array.mapi (fun i x -> M.sub x v2.(i)) v1


  let vdot v1 v2 =
    Array.fold_left M.plus M.init (Array.mapi (fun i x -> M.mult x v2.(i)) v1)

  let mvmul m v =
    let col = Array.length m.(0) in
    let r = Array.length v in
    let c = Array.make col M.init in
    for j = 0 to col-1 do
      for i = 0 to r-1 do
        c.(j) <- M.plus c.(j) (M.mult m.(i).(j) v.(i))
      done;
    done;
    c

  let sub_left m row1 row2 col1 col2 =
  (*  match m with
    | [|[||]|] -> [|[||]|]
    | _ -> init (row2 - row1 +1 ) (col2 - col1 +1) (fun i j -> get m i j) *)
    (*let m1 = create (col2 - col1 + 1) (row2 - row1 +1) m.(0).(0) in
    let r = row_dim m1 in
    let c = col_dim m1 in
    Printf.printf "R: %d C: %d\n" r c;
    for i=0 to r-1 do
      for j=0 to c-1 do
        Printf.printf "NEWR: %d NEWC: %d OLDR: %d OLDC:%d\n" i j (i+row1) (j+col1);
        flush stdout;
(*        m1.(i).(j) <- m.(i+row1).(j+col1); *)
        unsafe_set m1 i j m.(i+row1).(j+col1);
      done;
    done;
    m1*)
    (init (col2 - col1 + 1) (row2 - row1 + 1) (fun i j -> get m (i+row1) (j+col1)))



  let iter f m =
    let row_dim = Array.length m.(0) in
    for i=0 to (Array.length m) - 1 do
      for j=0 to row_dim - 1 do
	f (unsafe_get m i j)
      done
    done

  let iterij f m =
    let row_dim = Array.length m.(0) in
    for i=0 to (Array.length m) - 1 do
      for j=0 to row_dim - 1 do
	f i j (unsafe_get m i j)
      done
    done

  let map f m =
    match m with
    | [|[||]|] -> [|[||]|]
    | m ->
      let col_dim = col_dim m in
      let row_dim = row_dim m in
      let res = Array.create col_dim [||] in
      for i=0 to col_dim - 1 do
	let line = Array.unsafe_get m i in
	let first = f (Array.unsafe_get line 0) in
	let new_line = Array.create row_dim first in
	Array.unsafe_set res i new_line;
	for j=1 to row_dim - 1 do
	  Array.unsafe_set new_line j (f (Array.unsafe_get line j))
	done
      done;
	    res

  let mapij (f : int -> int -> 'a -> 'b) = function
    | [|[||]|] -> [|[||]|]
    | m ->
      let col_dim = col_dim m in
      let row_dim = row_dim m in
      let res = Array.create col_dim [||] in
      for i=0 to col_dim - 1 do
	let line = Array.unsafe_get m i in
	let first = f i 0 (Array.unsafe_get line 0) in
	let new_line = Array.create row_dim first in
	Array.unsafe_set res i new_line;
	for j=1 to row_dim - 1 do
	  Array.unsafe_set new_line j (f i j (Array.unsafe_get line j))
	done
      done;
      res

  let modif f = function
    | [|[||]|] -> ()
    | m ->
      let col_dim = col_dim m in
      let row_dim = row_dim m in
      for i=0 to col_dim - 1 do
	let line = Array.unsafe_get m i in
	for j=0 to row_dim - 1 do
	  Array.unsafe_set line j (f (Array.unsafe_get line j))
	done;
      done

  let modifij (f : int -> int -> 'a -> 'a) = function
    | [|[||]|] -> ()
    | m ->
      let col_dim = col_dim m in
      let row_dim = row_dim m in
      for i=0 to col_dim - 1 do
	let line = Array.unsafe_get m i in
	for j=0 to row_dim - 1 do
	  Array.unsafe_set line j (f i j (Array.unsafe_get line j))
	done;
      done


  let map2 (f : 'a -> 'b -> 'c) m1 m2 =
    let col_dim1 = col_dim m1 in
    let row_dim1 = row_dim m1 in
    if (col_dim1 <> col_dim m2) || (row_dim1 <> row_dim m2) then
      raise Not_same_dimension
    else
      match m1 with
      | [|[||]|] -> [|[||]|]
      | _ ->
	let res = Array.create col_dim1 [||] in
	for i=0 to col_dim1 - 1 do
	  let line1 = Array.unsafe_get m1 i in
	  let line2 = Array.unsafe_get m2 i in
	  let first = f (Array.unsafe_get line1 0) (Array.unsafe_get line2 0) in
	  let new_line = Array.create row_dim1 first in
	  Array.unsafe_set res i new_line;
	  for j=1 to row_dim1 - 1 do
	    Array.unsafe_set new_line j (f (Array.unsafe_get line1 j) (Array.unsafe_get line2 j))
	  done
	done;
	res

  let fold_left (f : 'b -> 'a -> 'b) x = function
    | [|[||]|] -> raise Empty_matrix
    | m ->
      let res = ref x in
      let col_dim = col_dim m in
      let row_dim = row_dim m in
      for i=0 to col_dim - 1 do
	let line = Array.unsafe_get m i in
	for j=0 to row_dim - 1 do
	  res := f !res (Array.unsafe_get line j)
	done
      done;
      !res

  let fold_right (f : 'a -> 'b -> 'b) m x = match m with
    | [|[||]|] -> raise Empty_matrix
    | m ->
      let res = ref x in
      let col_dim = col_dim m in
      let row_dim = row_dim m in
      for i = col_dim - 1 downto 0 do
	let line = Array.unsafe_get m i in
	for j = row_dim - 1 downto 0 do
	  res := f (Array.unsafe_get line j) !res
	done
      done;
      !res

  let identity n =
    let m = create n n 0. in
    for i=0 to n-1 do
      m.(i).(i) <- 1.
    done;
    m

  let random_int col_dim row_dim range =
    Random.self_init ();
    let int () =
      if Random.bool () then Random.int range
      else - (Random.int range) in
    let res = Array.create col_dim [||] in
    for i=0 to col_dim - 1 do
      let new_line = Array.create row_dim (int()) in
      Array.unsafe_set res i new_line;
      for j=1 to row_dim - 1 do
	Array.unsafe_set new_line j (int())
      done
    done;
    res

  let print_int a =
    let print_line line = Array.iter (printf "%d ") line; printf "\n" in
    Array.iter print_line a

  let println_int a =
    print_int a;
    printf "\n"
end
