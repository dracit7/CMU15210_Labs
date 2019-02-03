functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = unit table table

  fun makeCountTable (S : point seq) : countTable =
    if Seq.length S = 0 then OrdTable.empty() else
      let
        val get_a_key = #1 (Seq.nth S 0)
        (* Get something with type Key.t to pass the type matching of sml *)
        val sortedSeq = OrdTable.toSeq (OrdTable.map (OrdTable.fromSeq o Seq.map (fn x=>(x,()))) (OrdTable.collect S))
        val seqWithYs = Seq.scani (fn (a,b)=>(#1 b,OrdTable.merge (fn (x,_)=>x) (#2 a,#2 b))) (get_a_key,OrdTable.empty()) (Seq.rev sortedSeq)
      in
        OrdTable.fromSeq seqWithYs
      end

      (* OrdTable.map OrdTable.fromSeq (OrdTable.map (Seq.map (fn x=>(x,()))) (OrdTable.collect S)) *)

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
    let
      val leftPoints = case OrdTable.previous T xLeft of NONE => OrdTable.empty()
        | SOME(key,value) => value
      val notRightPoints = case OrdTable.split(T,xRght) of (_,SOME(value),_) => value 
        | (l,NONE,_) => case OrdTable.last l of SOME(key,value) => value
          | NONE => OrdTable.empty()
      fun countPoints(T) = OrdTable.size (OrdTable.getRange T (yLo,yHi))
    in
      countPoints notRightPoints - countPoints leftPoints
    end

    (* let
      fun getRangeY (T : unit table) =
        getRange T (yLo,yHi)
      val trimmedTable = OrdTable.getRange T (xLeft,xRght)
    in
      OrdTable.reduce op+ 0 (OrdTable.map (OrdTable.size o getRangeY) trimmedTable)
    end *)

end