functor MkListElt (structure Elt : ELEMENT) =
struct
  exception NYI

  val default = []
  type t = Elt.t list
  val equal = op=
  fun compare _ = raise NYI
  fun hash _ = raise NYI
  fun toString xs =
      "[" ^ String.concatWith "," (List.map Elt.toString xs) ^ "]"
end
