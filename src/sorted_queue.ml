
(* a queue so that jobs done coming in a random order are put back in input order
   before being muxed; for Mux.Sort_cat_into mux mode *)

type 'a t = { mutable next_rank: int;
              mutable q: (int * 'a) list }

let create () =
  let next_rank = 0 in
  let q = [] in
  { next_rank; q }

let insert_into (rank, x) l =
  let rec loop acc = function
    | [] -> List.rev_append acc [(rank, x)]
    | (rank', y) :: rest ->
      if rank' < rank then
        loop ((rank', y) :: acc) rest
      else if rank' = rank then
        assert(false) (* no two elements are allowed to have the same prio *)
      else (* rank' > rank *)
        List.rev_append acc ((rank, x) :: (rank', y) :: rest)
  in
  loop [] l

let insert (rank, elt) q =
  q.q <- insert_into (rank, elt) q.q

let pop q =
  match q.q with
  | [] -> None
  | (rank, elt) :: xs ->
    if rank <> q.next_rank then None
    else
      let res = Some elt in
      q.q <- xs;
      q.next_rank <- q.next_rank + 1;
      res

let pop_all q =
  let rec loop acc =
    match pop q with
    | None -> List.rev acc
    | Some x -> loop (x :: acc) in
  loop []
