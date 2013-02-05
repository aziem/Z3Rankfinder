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

module type Matrix =
  sig
    type t
    val id : t -> t
    val of_array : t array array -> t matrix
    val to_array : t matrix -> t array array
    val copy : t array array -> t array array
    val col_dim : t matrix -> int
    val row_dim : t matrix -> int
    val unsafe_set : t matrix -> int -> int -> t -> unit
    val get : t matrix -> int -> int -> t
    val unsafe_get : t matrix -> int -> int -> t
    val create : int -> int -> t -> t matrix
    val init : int -> int -> (int -> int -> t) -> t matrix
    val multiply : t matrix -> t matrix -> t matrix
    val scalar_multiply : t -> t matrix -> t matrix
    val add : t matrix -> t matrix -> t matrix
    val vminus : t array -> t array -> t array
    val mvmul : t matrix -> t array -> t array

    val sub_left : t matrix -> int -> int -> int -> int -> t matrix
    val iter : (t -> t) -> t matrix -> unit
    val iterij : (int -> int -> t -> t) -> t matrix -> unit
    val map : (t -> 'a) -> t matrix -> 'a matrix
    val mapij : (int -> int -> t -> t) -> t matrix -> t matrix
    val modif : (t -> t) -> t matrix -> unit
    val modifij : (int -> int -> t -> t) -> t matrix -> unit
    val map2 :
      (t -> t -> 'c) -> t matrix -> t matrix -> 'c matrix
    val fold_left : (t -> t -> t) -> t -> t matrix -> t
    val fold_right : (t -> t -> t) -> t matrix -> t -> t
    val identity : int -> float matrix
    val transpose : t matrix -> t matrix
    val random_int : int -> int -> int -> int matrix
    val print_int : int matrix -> unit
    val println_int : int matrix -> unit
  end

module Make (MatType : MATRIX_TYPE) : Matrix with type t = MatType.t
