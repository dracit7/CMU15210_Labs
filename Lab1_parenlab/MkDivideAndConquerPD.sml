functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq
  open Option210

  fun parenDist (parens : paren seq) : int option =
    let
      fun Solution parens =
        case showt parens of
          EMPTY => (nil,nil,NONE,0)
        | ELT (OPAREN) => (nil,[1],NONE,1)
        | ELT (CPAREN) => ([1],nil,NONE,1)
        | NODE (l,r) =>
            let 
              val ((LC,LO,Lmax,Llen),(RC,RO,Rmax,Rlen)) = 
                par((fn () => Solution l),(fn () => Solution r))
              fun Append(list,num) = 
                List.map (fn x=>x+num) list
              val MaxDist = intMax(Lmax,Rmax)
              val NewLength = Llen+Rlen
            in
              case (LO,RC) of
                (nil,nil) => (LC,RO,MaxDist,NewLength)
              | (nil,_)   => (LC @ Append(RC,Llen),RO,MaxDist,NewLength)
              | (_,nil)   => (LC,RO @ Append(LO,Rlen),MaxDist,NewLength)
              | (_,_) =>
                let
                  val (LenL,LenR) = (List.length(LO),List.length(RC))
                  val L = if LenL>=LenR then LC
                          else LC @ Append(List.drop(RC,LenL),Llen)
                  val R = if LenL<=LenR then RO
                          else RO @ Append(List.drop(LO,LenR),Rlen)
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
    end

end
