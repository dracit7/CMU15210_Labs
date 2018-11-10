functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives
  exception NotYetImplemented

  infix 6 ++ --
  infix 7 **

  fun printMessage(bitseq,0) = 0
    | printMessage(bitseq,n) =
      if (nth bitseq (length(bitseq)-n)) = ONE then
        let val p=print("1") in printMessage(bitseq,n-1) end
      else let val p=print("0") in printMessage(bitseq,n-1) end

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y = 
    case (length x,length y) of
      (0,_) => empty()
    | (_,0) => empty()
    | (1,_) => if nth x 0 = ONE then y else empty()
    | (_,1) => if nth y 0 = ONE then x else empty()
    | (LenX,LenY) => if LenX>LenY then y**x else
      let
        fun Shift(x,0) = x
          | Shift(x,n) = append(singleton(ZERO),Shift(x,n-1))
        val (q,p) = (take(x,LenX div 2),drop(x,LenX div 2))
        val (s,r) = (take(y,LenX div 2),drop(y,LenX div 2))
        val (pr,multi,qs) = par3((fn ()=>p**r),(fn ()=>(p++q)**(r++s)),(fn ()=>q**s))
        val ps_Plus_rq = multi -- pr -- qs
      in
        Shift(pr,LenX div 2 * 2) ++ Shift(ps_Plus_rq,LenX div 2) ++ qs
      end
	       
  val mul = op**
end
