functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  structure Key = Tree.Key
  (* This is defined after "open" so it doesn't get overwritten *)
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)

  fun first (T : 'a table) : (key * 'a) option =
    case Tree.expose T of NONE => NONE
    | SOME {key, value, left, right} =>
      case Tree.expose left of NONE => SOME(key,value)
        | _ => first left

  fun last (T : 'a table) : (key * 'a) option =
    case Tree.expose T of NONE => NONE
    | SOME {key, value, left, right} =>
      case Tree.expose right of NONE => SOME(key,value)
        | _ => last right
		      
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    case Tree.splitAt(T,k) of (l,_,_) => last l

  fun next (T : 'a table) (k : key) : (key * 'a) option =
    case Tree.splitAt(T,k) of (_,_,r) => first r

  fun join (L : 'a table, R : 'a table) : 'a table =
    Tree.join(L,R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    Tree.splitAt(T,k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    Table.filterk (fn (k,v) => if Key.compare(k,low) = LESS then false
                            else if Key.compare(k,high) = GREATER then false
                            else true) T
						       

end
