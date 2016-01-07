
(* there is a 

   functor Dict(K : ORDERED) : DICT 

*)

signature DICT =
sig
  structure Key : ORDERED
  type 'v dict 

  val empty   : 'v dict
  val insert  : 'v dict -> (Key.t * 'v) -> 'v dict

  val lookup  : 'v dict -> Key.t -> 'v option
  (* assumes key is in the dictionary *)
  val lookup' : 'v dict -> Key.t -> 'v 

  (* number of (key,value) pairs in the dictionary *)
  val size    : 'v dict -> int

  (* merge combine (d1,d2) == d where
     - k in d if and only if k is in d1 or k is in d2
     - If k~v in d1 and k is not in d2, then k ~ v in d
     - If k~v in d2 and k is not in d1, then k ~ v in d
     - If k~v1 in d1 and k~v2 in d2, then k ~ combine (v1, v2) in d
     *)
  val merge  : ('v * 'v -> 'v) -> 'v dict * 'v dict -> 'v dict
      
  (* computes the sequence of all (key,value) pairs in the dictionary,
     ordered from smallest key to largest key
     *)
  val toSeq : 'v dict -> (Key.t * 'v) Seq.seq  

  val map : ('a -> 'b) -> 'a dict -> 'b dict

end

