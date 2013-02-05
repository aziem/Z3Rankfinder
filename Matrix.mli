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

module Matrix (M : MATRIX_TYPE) :
  sig
    val of_array : M.t array array -> M.t matrix
    val to_array : M.t matrix -> M.t array array
    val copy : M.t array array -> M.t array array
    val col_dim : M.t matrix -> int
    val row_dim : M.t matrix -> int
    val unsafe_set : M.t matrix -> int -> int -> M.t -> unit
    val get : M.t matrix -> int -> int -> M.t
    val unsafe_get : M.t matrix -> int -> int -> M.t
    val create : int -> int -> M.t -> M.t matrix
    val init : int -> int -> (int -> int -> M.t) -> M.t matrix
    val multiply : M.t matrix -> M.t matrix -> M.t matrix
    val scalar_multiply : M.t -> M.t matrix -> M.t matrix
    val add : M.t matrix -> M.t matrix -> M.t matrix
    val vminus : M.t array -> M.t array -> M.t array
    val mvmul : M.t matrix -> M.t array -> M.t array

    val sub_left : M.t matrix -> int -> int -> int -> int -> M.t matrix
    val iter : (M.t -> M.t) -> M.t matrix -> unit
    val iterij : (int -> int -> M.t -> M.t) -> M.t matrix -> unit
    val map : (M.t -> M.t) -> M.t matrix -> M.t matrix
    val mapij : (int -> int -> M.t -> M.t) -> M.t matrix -> M.t matrix
    val modif : (M.t -> M.t) -> M.t matrix -> unit
    val modifij : (int -> int -> M.t -> M.t) -> M.t matrix -> unit
    val map2 :
      (M.t -> M.t -> 'c) -> M.t matrix -> M.t matrix -> 'c matrix
    val fold_left : (M.t -> M.t -> M.t) -> M.t -> M.t matrix -> M.t
    val fold_right : (M.t -> M.t -> M.t) -> M.t matrix -> M.t -> M.t
    val identity : int -> float matrix
    val transpose : M.t matrix -> M.t matrix
    val random_int : int -> int -> int -> int matrix
    val print_int : int matrix -> unit
    val println_int : int matrix -> unit
  end
