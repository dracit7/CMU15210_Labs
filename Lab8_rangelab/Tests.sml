structure Tests =
struct
  structure Seq = ArraySequence
  open Seq

  type point = int * int

  (* Here are a couple of test structures. ordSet1 is a test ordered set
   * (table), while points1 is a test collection of points in the plane.
   *
   * Note that for an ordered table test, you should just specify a sequence
   * of keys (and values will be automatically set to unit). *)

  val ordSet1 = % [5, 7, 2, 8, 9, 1]
  val ordSet2 = % [4, 6, 8, 7, 6, 2]
  val ordSet3 = % [~2, 10000, 3]

  val testsFirst = [
    ordSet1,
    ordSet2,
    ordSet3,
    % []
  ]
  val testsLast = [
    ordSet1,
    ordSet2,
    ordSet3,
    % []
  ]
  val testsPrev = [
    (ordSet1, 8),
    (ordSet1, 1),
    (ordSet2, 6),
    (ordSet3,~2),
    (% [], 8)
  ]
  val testsNext = [
    (ordSet1, 8),
    (ordSet1, 9),
    (ordSet2, 6),
    (ordSet3, 10000),
    (% [], 8)
  ]
  val testsJoin = [
    (ordSet1, % [100]),
    (ordSet1, % [3]),
    (ordSet2, ordSet3),
    (% [], % [100])
  ]
  val testsSplit = [
    (ordSet1, 7),
    (ordSet1, 100),
    (ordSet3, ~2),
    (% [], 7)
  ]
  val testsRange = [
    (ordSet1, (5,8)),
    (ordSet1, (10,12)),
    (ordSet2, (8,8)),
    (ordSet3, (~2,9)),
    (% [], (5,8))
  ]


  val points1 = % [(0,0),(1,2),(3,3),(4,4),(5,1)]
  val points2 : point seq = % []
  val points3 = % [(10000,10000),(0,0)]
  val points4 = tabulate (fn i => (i,i)) 1000
  val points5 = % [(3,4),(3,7),(8,1),(8,4)]
  val points6 = tabulate (fn i => (3,i)) 1000

  val testsCount = [
    (points1, ((1,3),(5,1))),
    (points1, ((2,4),(4,2))),
    (points1, ((100,101),(101,100))),

    (points2, ((0,10),(10,0))),
    (points3, ((0,10000),(10000,0))),
    (points4, ((0,500),(1000,0))),

    (points5, ((0,5),(8,4))),
    (points6, ((3,3),(3,3)))
  ]


end
