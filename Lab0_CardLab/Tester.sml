
structure Tester =
struct
  open StudentTestSuite

  type 'a sequence  = 'a ArraySequence.seq
  
  val tests = Tests.tests

  (* * * stringifier * * *)
  structure InElt = MkTripleElt (structure EltA = MkListElt(structure Elt = CardElt)
                                 structure EltB =  MkListElt(structure Elt = MoveElt)
                                 structure EltC = IntElt)
  structure InSeqElt = MkSeqElt (structure Elt = InElt
                                  structure Seq = ArraySequence)

  val stringifier = Logger.create (InElt.toString, IntElt.toString)

  (* * * running the tests * * *)
  fun testCard () =
    let
      val checker = Checker.fromRefsol (CardProblem.officiate,
                                        ReferenceCard.officiate,
                                        IntElt.equal)
    in
      Tester.testGroup checker stringifier tests
    end

end
