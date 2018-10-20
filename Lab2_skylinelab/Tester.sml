structure Tester =
struct
  open StudentTestSuite

  type 'a sequence  = 'a ArraySequence.seq
  
  val tests = Tests.tests

  structure OurSkyline = MkReferenceSkyline(structure S = ArraySequence)
  structure StuSkyline = MkSkyline(structure S = ArraySequence)
  (* * * stringifier * * *)
  structure OutSeqElt = MkSeqElt (structure Elt = MkPairElt(structure EltA = IntElt
                                                            structure EltB = IntElt)
                                  structure Seq = ArraySequence)

  structure InElt = MkTripleElt (structure EltA = IntElt
                                 structure EltB = IntElt
                                 structure EltC = IntElt)
  structure InSeqElt = MkSeqElt (structure Elt = InElt
                                  structure Seq = ArraySequence)

  val stringifier = Logger.create (InSeqElt.toString, OutSeqElt.toString)

  (* * * running the tests * * *)
  fun testSkyline () =
    let
      val checker = Checker.fromRefsol (StuSkyline.skyline,
                                        OurSkyline.skyline,
                                        OutSeqElt.equal)
    in
      Tester.testGroup checker stringifier tests
    end

end
