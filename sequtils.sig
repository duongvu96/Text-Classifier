
signature SEQUTILS =
sig
    val seq : string list -> string Seq.seq
    val words : string -> string Seq.seq

    val words_punc : string -> string Seq.seq

    val contains : ('a -> bool) -> 'a Seq.seq -> bool
    val strictSuffixes : 'a Seq.seq -> ('a Seq.seq) Seq.seq
        
    val explode : string -> char Seq.seq
    val implode : char Seq.seq -> string
end 
(* structure SeqUtils : SEQUTILS *)


signature SORT =
sig

    structure El : ORDERED
    val sort : El.t Seq.seq -> El.t Seq.seq

end
(* functor Sort(E : ORDERED) : SORT *)


