functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq
  open ASP

  exception NYI

  (* You must define the following type and
   * explain your decision here with a comment.
   *)
  type thesaurus = graph

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
    makeGraph (Seq.flatten (Seq.map (fn (word,syns)=>(Seq.map (fn x=>(word,x)) syns)) S))

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
    outNeighbors T w

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
    report (makeASP T w1) w2 

end
