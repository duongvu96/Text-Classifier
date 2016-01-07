
functor ExtractCombine (A : sig 
                                structure Key : ORDERED
                                structure MR : MAP_REDUCE
                            end) : EXTRACT_COMBINE =
struct
	structure MR : MAP_REDUCE = A.MR

	structure D : DICT = Dict(A.Key)

	fun extractcombine (extractor : 'a -> (D.Key.t * 'v) Seq.seq) (combiner : 'v * 'v -> 'v) (mr : 'a MR.mapreducable) = 
		MR.mapreduce 	(fn a => Seq.mapreduce (fn (k,v) => D.insert D.empty (k,v)) D.empty (D.merge combiner) (extractor a)) 
						D.empty 
						(D.merge combiner)
						mr
end

