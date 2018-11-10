functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  exception NotYetImplemented
  infix 6 ++ --
  fun x ++ y = BNA.add (x, y)
  fun x -- y =
    let
      fun AppendZero(x,0) = x
        | AppendZero(x,n) = append(AppendZero(x,n-1),singleton(ZERO))
      fun Prepare(x,y) =
        if length(x) > length(y) then (AppendZero(x,1),AppendZero(y,length(x)-length(y)+1))
        else (AppendZero(x,length(y)-length(x)+1),AppendZero(y,1))
      val (PreX,PreY) = Prepare(x,y)
      val Result = PreX ++ map (fn (x)=>if x=ONE then ZERO else ONE) PreY ++ singleton(ONE)
      (* x - y = x + (-y) = x + ~y + 1 *)
    in
      take(Result,length(Result)-2)
      (* Get rid of the sign bit *)
    end
      
  val sub = op--
end
