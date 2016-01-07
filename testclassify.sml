
functor TestClassify(Dataset : MAP_REDUCE) 
  : 
  sig
      type labeled_document
      type document 
      val print_stats          : labeled_document Dataset.mapreducable -> unit
      val print_stats_nofreqs  : labeled_document Dataset.mapreducable -> unit
      val print_possibles      : labeled_document Dataset.mapreducable -> labeled_document -> unit
      val number_correct       : labeled_document Dataset.mapreducable -> labeled_document Dataset.mapreducable -> int * int
      val print_predictions    : labeled_document Dataset.mapreducable -> labeled_document Dataset.mapreducable -> unit
  end
  =
struct
    structure C = NaiveBayes(struct structure Category = StringLt
                                    val default_category = "unknown"
                                    structure Dataset = Dataset
                             end)

    type labeled_document = C.labeled_document
    type document = C.document

    fun print_stats' (printfreqs : bool) (train : C.labeled_document Dataset.mapreducable) : unit =
        let 
            val (num_docs_by_cat,
                 num_words_by_cat,
                 freqs,
                 all_categories, 
                 total_num_docs,
                 total_num_words) = C.gather train 
                
            val () = print ("Number of documents by category:\n" ^ 
                            (Seq.mapreduce (fn (cat,count) => "  " ^ cat ^ " " ^ Int.toString count ^ "\n")
                                           "" (op^)
                                           (C.CatDict.toSeq num_docs_by_cat))
                            )

            val () = print ("Number of words by category:\n" ^ 
                            (Seq.mapreduce (fn (cat,count) => "  " ^ cat ^ " " ^ Int.toString count ^ "\n")
                                          "" (op^)
                                          (C.CatDict.toSeq num_words_by_cat))
                            )

            val _ = case printfreqs of 
                true =>
                    print ("Frequencies:\n" ^ 
                           Seq.mapreduce (fn ((cat, word),count) =>
                                          (("  " ^ cat ^ ": " ^  word ^ " occured " ^ Int.toString count ^ " times \n")))
                           "" (op^) 
                           (C.CatWordDict.toSeq freqs))
              | false => ()

            val () = print ("All categories: " ^ Seq.reduce (fn (c1,c2) => c1 ^ " " ^ c2) "" all_categories ^ "\n")
            val () = print ("Total number of documents:" ^ Int.toString total_num_docs ^ "\n")
            val () = print ("Total number of distinct words:" ^ Int.toString total_num_words ^ "\n")
        in 
            ()
        end

    val print_stats = print_stats' true
    val print_stats_nofreqs = print_stats' false

    fun print_possibles (train : C.labeled_document Dataset.mapreducable) ((cat, words) : C.labeled_document) : unit = 
        let val probs = C.possible_classifications (C.gather train) words
        in 
            print (("Correct Category: " ^ cat ^ "\n" ^
                    "Scores:\n" ^ Seq.mapreduce (fn (c,r) => "  " ^ c ^ " " ^ Real.toString r) "" (fn (s1,s2) => s1 ^ "\n" ^ s2) probs))
        end

    fun number_correct (train : labeled_document Dataset.mapreducable) (test : labeled_document Dataset.mapreducable) : int * int = 
        let 
            val cl = C.train_classifier train
        in 
            Dataset.mapreduce (fn (correct_answer, words) =>
                          let val (predicted, _) = cl words
                          in 
                              case predicted = correct_answer of
                                  true => (1,1)
                                | false => (0,1)
                          end) 
                          (0,0)
                          (fn ((cor1,tot1),(cor2,tot2)) => (cor1 + cor2 , tot1 + tot2))
                          test
        end

    fun print_predictions (train : labeled_document Dataset.mapreducable) (test : labeled_document Dataset.mapreducable) : unit =
        let 
            val cl = C.train_classifier train
        in 
            print (Dataset.mapreduce (fn (correct_answer, words) =>
                                 let val (predicted, _) = cl words
                                     val correctstring = "Given Categories: " ^ correct_answer ^ "\n"
                                     val predstring    = "Predicted: " ^ predicted ^ "\n"
                                     val doc           = (Seq.reduce (fn (s1,s2) => s1 ^ " " ^ s2) "" words) ^ "\n"
                                     val report        = correctstring ^ predstring ^ doc ^ "\n"
                                 in 
                                     (case correct_answer = predicted of
                                          true => "CORRECT\n" ^ report
                                        | false => "INCORRECT\n" ^ report)
                                 end) 
                                ""
                                (op^)
                                test)
        end
end

structure TestSeq =
struct

    structure TC = TestClassify(SeqMR)
    open TC

    val seq = Seq.fromlist

    val simplest_train = seq [ ("ECAT", seq ["stock"]), ("GCAT", seq ["congress"]) ]
    val simple_train   = seq [ ("ECAT", seq ["stock","price"]), ("GCAT", seq ["congress","court"]) ]
    val cross_train    = seq [ ("ECAT", seq ["stock","price","fell"]), ("GCAT", seq ["congress","court","fell"]) ]
    val dups_train     = seq [ ("ECAT", seq ["stock","price","stock","price"]), ("GCAT", seq ["congress","court","court","congress"]) ]

    val doc1 = ("ECAT", seq ["stock"])
    val doc2 = ("GCAT", seq ["congress"])
    val doc3 = ("GCAT", seq ["court","fell"])
    val doc4 = ("ECAT", seq ["stock","ticker"])

    val docs14 = seq[doc1,doc2,doc3,doc4]

end

structure TestFile =
struct

    structure TC = TestClassify(FileMR)
    open TC

    fun parse_line (s : string) : labeled_document option = 
        let val [cats , noncats] = String.tokens (fn #"\t" => true | _ => false) s
            val toplevel = (Seq.filter (fn s => String.isSuffix "CAT" s) (Seq.fromlist (String.tokens (fn #"," => true | _ => false) cats)))
        in 
            case Seq.length toplevel of
                0 => NONE
              | _ => SOME (Seq.nth 0 toplevel,
                           SeqUtils.words noncats)
        end

    fun open_file (s : string) : labeled_document FileMR.mapreducable  =
        (fn () => TextIO.openIn s, parse_line)

    fun time (f : unit -> 'a) : {gcsys : IntInf.int, gcusr : IntInf.int, ngcsys : IntInf.int, ngcusr : IntInf.int} * 'a = 
        let val t = Timer.startCPUTimer ()
            val a = f ()
            val {gc={sys=gcsys,usr=gcusr},
                 nongc={sys=ngcsys,usr=ngcusr}} = Timer.checkCPUTimes t
        in
            ({gcsys = Time.toSeconds gcsys, 
              gcusr = Time.toSeconds gcusr,
              ngcsys = Time.toSeconds ngcsys,
              ngcusr = Time.toSeconds ngcusr}, a)
        end

end
