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
    OrdTable.map OrdTable.fromSeq (OrdTable.map (Seq.map (fn x=>(x,()))) (OrdTable.collect S))

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
    let
      fun getRangeY (T : unit table) =
        getRange T (yLo,yHi)
      val trimmedTable = OrdTable.map getRangeY (OrdTable.getRange T (xLeft,xRght))
    in
      OrdTable.reduce op+ 0 (OrdTable.map OrdTable.size trimmedTable)
    end
end