
(* how to merge computation results *)

type filename = string

type t = Null
       | Cat_into of filename
       | Sorted_cat_into of filename
