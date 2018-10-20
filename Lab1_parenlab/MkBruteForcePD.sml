functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq
  open Option210

  (* 0 represents the first move, 1 represents others. *)
  fun parenMatch s =
    let
      fun pm ((NONE,_),_)         = (NONE,1)
        | pm ((SOME(0),_),CPAREN) = (NONE,1)
        | pm ((SOME(0),0),OPAREN) = (SOME(1),1)
        | pm ((SOME(0),1),OPAREN) = (NONE,1)
        | pm ((SOME(n),_),OPAREN) = (SOME(n+1),1)
        | pm ((SOME(n),_),CPAREN) = (SOME(n-1),1)
    in
      iter pm (SOME 0,0) s = (SOME 0,1)
    end
  (* This function is an extension of the formal parenMatch function.
   * To test if a parenseq is a properly matched one, we just need to
   * check the statcode. If a certain move (other than the first move
   * and the last move) makes the statcode become 0, this parenseq is
   * not properly matched although it might be matched in recitation's case.
   *)

  
  fun parenDist (parens : paren seq) : int option =
    let
      fun GetAllSubseq(parens,i)=
        let
          fun Helper(parens,i,base) =
            if i = base then nil
            else (subseq parens (base,i-base),SOME(i-base)) :: Helper(parens,i-1,base)
          (* Helper gets subsequences of parens from base-i to base-base. *)
        in
          if i = ~1 then nil
          else Helper(parens,length(parens),i) @ GetAllSubseq(parens,i-1)
          (* This function is equal to a nested loop. *)
        end
      fun Solution(nil) = NONE
        | Solution((Top,len)::SubseqList) = 
          if parenMatch Top then
            intMax (len,Solution(SubseqList))
          else Solution(SubseqList)
    in
      Solution(GetAllSubseq(parens,length(parens)))
    end


end
