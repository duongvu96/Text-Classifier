
functor NaiveBayes (ClassSpec : sig
                                  structure Category : ORDERED
                                  val default_category : Category.t
                                      
                                  structure Dataset : MAP_REDUCE
                                end) : NAIVE_BAYES_CLASSIFIER =
struct

    type category = ClassSpec.Category.t

    type labeled_document = category * string Seq.seq
    type document = string Seq.seq

    structure Dataset = ClassSpec.Dataset

    (* TASK instantiate the ExtractCombine functor 3 times, and 
            define CatDict and WordDict and CatWordDict 
            to be the dictionary modules this produces
    *)
    structure CatEC = ExtractCombine( struct
                                        structure Key = ClassSpec.Category
                                        structure MR = ClassSpec.Dataset
                                      end)
    structure WordEC = ExtractCombine(struct
                                        structure Key = StringLt
                                        structure MR = ClassSpec.Dataset
                                      end)
    structure CatWordEC = ExtractCombine( struct
                                            structure Key = PairOrder(struct
                                                                            structure O1 = ClassSpec.Category
                                                                            structure O2 = StringLt
                                                                          end)
                                            structure MR = ClassSpec.Dataset
                                          end)
    structure CatDict = CatEC.D
    structure WordDict = WordEC.D
    structure CatWordDict = CatWordEC.D

    type statistics = 
          int CatDict.dict     (* maps each category to number of documents with that category *)
        * int CatDict.dict     (* maps each category to number of words in documents with that category *)
        * int CatWordDict.dict (* maps each (cat,word) to frequency *)
        * category Seq.seq     (* list of categories (no duplicates) *)
        * int                  (* total number of documents *)
        * int                  (* total number of different words *)

    (* TASK *)
    fun gather (train : labeled_document Dataset.mapreducable) : statistics =
      let
        val cat_num_doc = CatEC.extractcombine (fn (a,b) => Seq.singleton (a, 1)) Int.+ train
        val cat_word_doc = CatEC.extractcombine (fn (a,b) => Seq.singleton (a, Seq.length b)) Int.+ train
        val catword_freq = CatWordEC.extractcombine (fn (a,b) => Seq.map (fn w => ((a,w),1)) b) Int.+ train
        val cat_num_seq = CatDict.toSeq cat_num_doc
        val cat_list = Seq.map (fn (a,b) => a) cat_num_seq
        val doc_count = Seq.reduce Int.+ 0 (Seq.map (fn (a,b) => b) cat_num_seq)
        val word_count = WordDict.size (WordEC.extractcombine (fn (a,b) => Seq.map (fn w => (w,1)) b) Int.+ train)
      in
        (cat_num_doc, cat_word_doc, catword_freq, cat_list, doc_count, word_count)
      end

    (* TASK *)
    fun possible_classifications 
        ((num_docs_by_cat,
          num_words_by_cat,
          freqs,
          all_categories, 
          total_num_docs,
          total_num_words) : statistics)
        (test_doc : document) : (category * real) Seq.seq = 
     Seq.map (fn c => (c, Math.ln(real(CatDict.lookup' num_docs_by_cat c) / real(total_num_docs)) + 
                      Seq.mapreduce (fn w => case CatWordDict.lookup freqs (c,w) of
                                                NONE => Math.ln(1.0 / real(total_num_words))
                                              | SOME n => Math.ln(real(CatWordDict.lookup' freqs (c,w)) / real(CatDict.lookup' num_words_by_cat c))) 
                                      0.0 (fn (x,y) => x + y) test_doc)) all_categories

    (* TASK *)
    fun classify (stats : statistics)
                 (test_doc : document) : (category * real) =
      Seq.reduce (fn ((a,b), (c,d)) => case b < d of
                                          true => (c,d)
                                        | _ => (a,b))
                  (ClassSpec.default_category, Real.negInf)
                  (possible_classifications stats test_doc)

    (* TASK *)
    fun train_classifier (train : labeled_document Dataset.mapreducable) : document -> (category * real) =
        classify (gather train)
        
end
