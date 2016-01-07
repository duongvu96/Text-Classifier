signature CLASSIFIER = 
sig
    structure Dataset : MAP_REDUCE

    type category

    type document = string Seq.seq
    type labeled_document = category * document
        
    val train_classifier : labeled_document Dataset.mapreducable 
                         -> document -> (category * real)
end

signature NAIVE_BAYES_CLASSIFIER = 
sig
    structure Dataset : MAP_REDUCE

    type category

    type labeled_document = category * string Seq.seq
    type document = string Seq.seq
        
    val train_classifier : labeled_document Dataset.mapreducable -> document -> (category * real)


    (* ---------------------------------------------------------------------- *)
    (* internal components that are exported only for testing *)

    structure CatDict : DICT
    structure CatWordDict : DICT

    type statistics = 
          int CatDict.dict     (* maps each category to number of documents with that category *)
        * int CatDict.dict     (* maps each category to number of words in documents with that category *)
        * int CatWordDict.dict (* maps each (cat,word) to frequency *)
        * category Seq.seq     (* list of categories (no duplicates) *)
        * int                  (* total number of documents *)
        * int                  (* total number of different words *)

    val gather : labeled_document Dataset.mapreducable -> statistics 

    val possible_classifications : statistics -> document -> (category * real) Seq.seq
    val classify : statistics -> document -> category * real

end 

