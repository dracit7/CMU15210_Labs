structure Tests =
struct

  structure T = MkTreapTable(structure HashKey = IntElt)
  structure S = T.Seq
  open S

  type edge = int * int

  (*a trivial test that has a graph containing 2 vertices and an edge*)
  val edgeseq = [(1,2)]
  val edgeseq2 = [(1,2),(2,3),(3,4),(2,4),(1,5),(5,4),(5,6),(6,7)]
  val test3 = [(2,1),(4,2),(4,3),(4,5),(1,3),(3,5),(3,2),(1,4),(2,5),(5,1)]
  val testfile = "input/thesaurus.txt"
  val testfile2 = "input/simpletest.txt"

  (* The following are required *)
  val testsNum = [edgeseq, edgeseq2,test3];

  val testsOutNeighbors = [(edgeseq, 1), (edgeseq, 2),(test3,1),(edgeseq2,5),(edgeseq2,7),(test3,9)]

  val testsReport = [((edgeseq, 1), 2), ((edgeseq2, 1), 4), ((edgeseq2, 1), 7),((test3,4),2),((test3,1),3),((test3,6),2),((test3,1),6)]

  val testsNumWords =  [testfile, testfile2]

  val testsSynonyms =
    [(testfile2, "HANDSOME"),
     (testfile2, "VINCENT"),
     (testfile2, "PRETTY"),
	 (testfile2,"BADASS"),
     (testfile, "GOOD")]

  val testsQuery =
    [(testfile2, ("HANDSOME", "YOLO")),(testfile2,("BADASS","STUPID")),(testfile2,("PRETTY","CHRIS")),(testfile, ("GOOD", "BAD")),(testfile, ("CLEAR", "VAGUE")),(testfile, ("LOGICAL", "ILLOGICAL")),(testfile,("HAPPY","SAD")),(testfile,("LIBERAL","CONSERVATION")),(testfile, ("EARTHLY", "POISON"))]

end
