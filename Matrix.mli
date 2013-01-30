type 'a matrix = 'a array array
exception Not_a_matrix
exception Not_same_dimension
exception Empty_matrix
exception Wrong_dimension
external to_array : 'a matrix -> 'a array array = "%identity"
module Matrix :
  sig
    val of_array : 'a array array -> 'a matrix
    val copy : 'a array array -> 'a array array
    val col_dim : 'a matrix -> int
    val row_dim : 'a matrix -> int
    val unsafe_set : 'a matrix -> int -> int -> 'a -> unit
    val get : 'a matrix -> int -> int -> 'a
    val unsafe_get : 'a matrix -> int -> int -> 'a
    val create : int -> int -> 'a -> 'a matrix
    val init : int -> int -> (int -> int -> 'a) -> 'a matrix
    val sub_left : 'a matrix -> int -> int -> int -> int -> 'a matrix
    val iter : ('a -> 'b) -> 'a matrix -> unit
    val iterij : (int -> int -> 'a -> 'b) -> 'a matrix -> unit
    val map : ('a -> 'b) -> 'a matrix -> 'b matrix
    val mapij : (int -> int -> 'a -> 'b) -> 'a matrix -> 'b matrix
    val modif : ('a -> 'a) -> 'a matrix -> unit
    val modifij : (int -> int -> 'a -> 'a) -> 'a matrix -> unit
    val map2 :
      ('a -> 'b -> 'c) -> 'a matrix -> 'b matrix -> 'c matrix
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b matrix -> 'a
    val fold_right : ('a -> 'b -> 'b) -> 'a matrix -> 'b -> 'b
    val identity : int -> float matrix
    val transpose : 'a matrix -> 'a matrix
    val random_int : int -> int -> int -> int matrix
    val print_int : int matrix -> unit
    val println_int : int matrix -> unit
  end
