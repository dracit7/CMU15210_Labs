functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq
  open Option210

  fun parenDist (parens : paren seq) : int option =
  (* Complexity of this algorithm: W(n)=2W(n/2)+Wshowt(n)+O(1) *)

    (* Use a tuple with size 5 to pass all possible information: 
     * maxDist: present max distance
     * Closes: number of unmatched Cparens
     * Opens: number of unmatched Oparens
     * leftLen: the distance between the rightmost unmatched Cparen and the left edge
     * rightLen: the distance between the leftmost unmatched Oparen and the right edge
     *)
    let
      fun Dist parens = 
        case showt parens of
          EMPTY => (SOME 0,0,0,0,0)
        | ELT(OPAREN) => (SOME 0,0,1,0,1)
        | ELT(CPAREN) => (SOME 0,1,0,1,0)
        | NODE (l,r) =>
          let
            val ((Lmax,LCs,LOs,Lleft,Lright),(Rmax,RCs,ROs,Rleft,Rright)) =
              par((fn _ => Dist l),(fn _ => Dist r))
            val maxDist = intMax(intMax(Lmax,Rmax),SOME(Lright+Rleft))
            val SOME(maxLR) = intMax(Lmax,Rmax)
          in
            if LOs = RCs then (maxDist,LCs,ROs,Lleft,Rright)
            else if LOs < RCs then (intMax(Lmax,Rmax),LCs+RCs-LOs,ROs,Rleft+length l,Rright)
            else (intMax(Lmax,Rmax),LCs,LOs-RCs+ROs,Lleft,Lright+length r)
          end
      val (Result,_,_,_,_) = Dist parens
    in
      if Result = SOME 0 then NONE
      else Result
    end
    
    (* The former version, which holds the complexity W(n)=2W(n/2)+Wshowt(n)+O(n) *)
    (* let
      fun Solution parens =
        case showt parens of
          EMPTY => (nil,nil,NONE,0)
        | ELT (OPAREN) => (nil,[1],NONE,1)
        | ELT (CPAREN) => ([1],nil,NONE,1)
        | NODE (l,r) =>
            let 
              val ((LC,LO,Lmax,Llen),(RC,RO,Rmax,Rlen)) = 
                par((fn () => Solution l),(fn () => Solution r))
              fun SeqAdd(list,num) = 
                List.map (fn x=>x+num) list
              val MaxDist = intMax(Lmax,Rmax)
              val NewLength = Llen+Rlen
            in
              case (LO,RC) of
                (nil,nil) => (LC,RO,MaxDist,NewLength)
              | (nil,_)   => (LC @ SeqAdd(RC,Llen),RO,MaxDist,NewLength)
              | (_,nil)   => (LC,RO @ SeqAdd(LO,Rlen),MaxDist,NewLength)
              | (_,_) =>
                let
                  val (LenL,LenR) = (List.length(LO),List.length(RC))
                  val L = if LenL>=LenR then LC
                          else LC @ SeqAdd(List.drop(RC,LenL),Llen)
                  val R = if LenL<=LenR then RO
                          else RO @ SeqAdd(List.drop(LO,LenR),Rlen)
                  val Temp = (if LenL<LenR then LenL else LenR)-1
                  val Max = SOME(List.nth(LO,Temp)+List.nth(RC,Temp))
                in
                  (L,R,intMax(Max,MaxDist),NewLength)
                end
            end
    in
      let
        val (_,_,distance,_) = Solution parens
      in
        distance
      end
    end *)

end
