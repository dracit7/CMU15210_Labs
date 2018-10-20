functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  exception NotYetImplemented
  
  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
    let
      datatype side = LEFT | RIGHT
      fun Combine(LHeights,RHeights) =
        let
          fun HelpCmp((i,_,_),(j,_,_)) =
            if i<j then LESS
            else if i=j then EQUAL
            else GREATER
          (* The Compare function for merge() *)
          fun AttachSide Side Heights = map (fn (x,h)=>(x,h,Side)) Heights
          (* Use "side" to identify different buildings. *)
          val Heights = merge HelpCmp (AttachSide LEFT LHeights) (AttachSide RIGHT RHeights)
          (* Sort heights by their x axes. *)
          fun GenPart Side (PresentBuilding,NextBuilding) =
            let
              val (PreX,PreHeight,PreSide) = PresentBuilding
              val (NextX,NextHeight,NextSide) = NextBuilding
            in
              if NextSide=Side then NextBuilding
              (* For buildings of one side, they're the structure of
               * the skyline, so keep their heights. *)
              else (NextX,PreHeight,PreSide)
              (* As for buildings of the other side, set their height
               * to PreHeight. If PreHeight equals 0, which means this
               * Xaxis is out of another building, the final value would
               * still be NextHeight because of the next phase's comparation.
               * If PreHeight is the height of another building, which
               * means this Xaxis is in the middle of another building,
               * PreHeight and NextHeight would be compared in the next
               * phase to decide which is part of the skyline.
               *)
            end
          fun GenSkyline(Side) = scani (GenPart Side) (0,0,Side) Heights
          fun MergeSkyline(nil,nil) = []
            | MergeSkyline(LTop::LHeights,RTop::RHeights) =
              let
                val (LX,LHeight,_) = LTop
                val (_,RHeight,_) = RTop
              in
                if LHeight<RHeight then (LX,RHeight)::MergeSkyline(LHeights,RHeights)
                else (LX,LHeight)::MergeSkyline(LHeights,RHeights)
              end
          (* Compare two parts of the skyline and merge them. *)
          fun Filter(nil) = nil
            | Filter(Single::[]) = [Single]
            | Filter(Top::Sec::Rest) =
              let
                val (_,TopHeight) = Top
                val (_,SecHeight) = Sec
              in
                if TopHeight=SecHeight then Filter(Top::Rest)
                else Top::Filter(Sec::Rest)
              end
          (* Filter out redundant points *)
        in
          fromList(Filter(MergeSkyline(toList(GenSkyline LEFT),toList(GenSkyline RIGHT))))
        end
      fun Solution(buildings) = 
        case showt(buildings) of
          EMPTY => empty ()
        | ELT (Left,Height,Right) => fromList([(Left,Height),(Right,0)])
        | NODE (Llist,Rlist) =>
          let
            val (LHeights,RHeights) = par(
              fn _=>Solution(Llist),
              fn _=>Solution(Rlist)
            )
          in
            Combine(LHeights,RHeights)
          end
    in
      Solution buildings
    end
end
