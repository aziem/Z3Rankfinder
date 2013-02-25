(** Module to perform rank function synthesis.
*)

module Rankfinder :
  sig
    (**
       Rank function synthesis.
       Input:

       Matrix of integers representing a set of relations.
       Each line represents a relation over primed and unprimed variables.

       For example:

       [-1;0;0;0;1]
       represents:

       -1*x + 0*y + 0*x' + 0*y' + 1 <= 0

       (or x>0)

       Another example:
       [-1;0;1;0;1]
       -1*x + 0*y + 1*x' + 0*y' + 1 <=0

       (or x' >= x + 1, which means x has been incremented).

       Output:
       If rank function synthesis is successful Some(r,delta), otherwise None.

    *)
       val synthesis : int array array -> (int array * int array) option
  end
