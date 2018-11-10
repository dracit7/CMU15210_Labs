functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  infix 6 ++
  exception NotYetImplemented
  datatype carry = GEN | PROP | STOP

  fun printMessage(bitseq,0) = 0
    | printMessage(bitseq,n) =
      if (nth bitseq (length(bitseq)-n)) = ONE then
        let val p=print("1") in printMessage(bitseq,n-1) end
      else let val p=print("0") in printMessage(bitseq,n-1) end

  fun printCarry(carryseq,0) = 0
    | printCarry(carryseq,n) =
      case (nth carryseq (length(carryseq)-n)) of
        GEN => let val p=print("G") in printCarry(carryseq,n-1) end
      | PROP => let val p=print("P") in printCarry(carryseq,n-1) end
      | _ => let val p=print("S") in printCarry(carryseq,n-1) end

  (* Functions for testing *)

  fun x ++ y =
    let
      fun GenCarry(x,y) =
        case (x,y) of
          (ONE,ONE) => GEN
        | (ZERO,ZERO) => STOP
        | _ => PROP
      (* Generate carrys by x and y *)
      fun Copy(a,b) =
        if b=PROP then a
        else b
      (* Propagate means pass the previous carry *)
      fun GenBits(a,GEN) = if a=PROP then ZERO else ONE
        | GenBits(a,PROP) = if a=STOP then ONE else ZERO
        | GenBits(a,STOP) = if a=PROP then ONE else ZERO
      (* (a,GEN) means the bits before a generated a carry
       * (a,PROP) means the carry before a is not equal to a,
       * so if a is STOP then the carry before a is GEN, 
       * elsewise is STOP.
       * (a,STOP) means a doesn't have to carry *)
      fun AppendZero(x,0) = x
        | AppendZero(x,n) = append(AppendZero(x,n-1),singleton(ZERO))
      fun Prepare(x,y) =
        if length(x) > length(y) then (AppendZero(x,1),AppendZero(y,length(x)-length(y)+1))
        else (AppendZero(x,length(y)-length(x)+1),AppendZero(y,1))
      val (PreX,PreY) = Prepare(x,y)
      (* Make sure that length(x) = length(y) ;
       * Append one more ZERO to handle the carry of the rightmost bits. *)
      val RawCarrys = map2 GenCarry PreX PreY
      val Carrys = scani Copy STOP RawCarrys
      val ShiftedCarrys = append(singleton(STOP),take(Carrys,length(Carrys)-1))
      val Filter = map2 (fn (x,y)=>if x<>y then PROP else x) Carrys ShiftedCarrys
      val CarryMask = map2 (fn (x,y)=>if x=PROP then PROP else y) Filter Carrys
      (* Generate the CarryMask *)
      val Result = map2 GenBits RawCarrys CarryMask
    in
      Result
    end

    (* let
      fun BitsAdd(PrevBits,Bits) = 
        let
          val (_,_,Carry) = PrevBits
          val (bit_1,bit_2,_) = Bits
        in
          case (bit_1,bit_2) of
            (ONE,ONE) => 
              if Carry=STOP then (ZERO,ZERO,GEN)
              else (ONE,ZERO,GEN)
          | (ZERO,ZERO) =>
              if Carry=STOP then (ZERO,ZERO,STOP)
              else (ONE,ZERO,STOP)
          | (_,_) =>
              if Carry=STOP then (ONE,ZERO,STOP)
              else (ZERO,ZERO,GEN)
        end
      fun AppendZero(x,0) = x
        | AppendZero(x,n) = append(AppendZero(x,n-1),singleton(ZERO))
      fun Prepare(x,y) = 
        if length(x) > length(y) then map2 (fn (x,y)=>(x,y,STOP)) x (AppendZero(y,length(x)-length(y)))
        else map2 (fn (x,y)=>(x,y,STOP)) (AppendZero(x,length(y)-length(x))) y
      val result = scani BitsAdd (ZERO,ZERO,STOP) (Prepare(x,y))
    in
      if #3 (nth result ((length result)-1)) = GEN then
        map (fn (x,_,_)=>x) (append(result,singleton((ONE,ZERO,STOP))))
      else map (fn (x,_,_)=>x) result
    end *)
    
  val add = op++
end
