functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  type vertex = key
  (* Table.key defines our vertex type *)
  type edge = vertex * vertex

  exception NYI
  (* You must define the following two types and
   * explain your decision here with comments.
   *)
  type graph = (vertex * vertex seq) seq
  type asp = unit

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
  raise NYI

  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
  raise NYI

  fun numVertices (G : graph) : int =
  raise NYI

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    raise NYI
  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
  raise NYI

  (* Task 2.5 *)
fun report (A : asp) (v : vertex) : vertex seq seq =
 raise NYI

end
