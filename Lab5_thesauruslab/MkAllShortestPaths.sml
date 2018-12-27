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
  type graph = set table
  (* To increase the efficiency, this adjacent table uses set instead of list
   * to store vertexes that is adjacent to a certain vertex. *)
  type asp = (vertex seq) table
  (* The subpath of the shortest path is also the shortest path, so
   * we just need to keep the parent vertex of each vertex in the shortest path. *)

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
    let
      val body = Table.map Set.fromSeq (Table.collect E)
      val endPoints = Table.fromSeq (Seq.map (fn (beg,fin)=>(fin,Set.empty())) E)
      (* Add endPoints to the table in order to make it more convenience
       * when counting vertexes. *)
    in
      Table.merge (fn (body,_)=>body) (body,endPoints)
    end

  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
    Table.reduce op+ 0 (Table.map (fn x=>Set.size x) G)

  fun numVertices (G : graph) : int =
    Table.size G

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    case find G v of SOME(S) => Set.toSeq S | NONE=>Seq.empty()

  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    let
      fun BFS(Frontiers,PathEdges) =
        if Seq.length Frontiers = 0 then PathEdges else
          let
            val Expansion = Table.collect (Seq.flatten (Seq.map (fn v=>(Seq.map (fn u=>(u,v)) (outNeighbors G v))) Frontiers))
            (* Calculate the expansion part of the graph from the frontiers. *)
            val NewPathEdges = Table.merge (fn (a,_)=>a) (PathEdges,Expansion)
            (* Add this part to former pathedges. *)
            val NewFrontiers = Set.toSeq (Table.domain (Table.erase (Expansion,(Table.domain PathEdges))))
            (* New frontiers are the next side of the expansion. *)
          in
            BFS(NewFrontiers,NewPathEdges)
          end
    in
      BFS(Seq.singleton v,Table.singleton (v,Seq.empty()))
    end

  (* Task 2.5 *)
fun report (A : asp) (v : vertex) : vertex seq seq =
  case find A v of SOME(inNeighbors) =>
    if length inNeighbors = 0 then Seq.singleton (Seq.singleton v) else
    Seq.map (fn path=>Seq.append(path,Seq.singleton(v))) (Seq.flatten (Seq.map (fn v=>report A v) inNeighbors))
  | NONE => Seq.empty()

end
