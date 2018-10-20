structure MoveElt : ELEMENT =
struct
open CardType
exception NYI
type t = move

  val default = Draw
  val equal = op=
  fun compare (x,y)= raise NYI
  fun hash i = raise NYI
  fun toString x =
  let
    fun cardToString (suit,rank) =
  let
    fun rank_to_string x = case x of
        Jack=> "Jack" | Queen=> "Queen" | King=> "King" | Ace=>"Ace" | Num n => Int.toString n
    in
    case suit of
        Clubs => "Clubs "^rank_to_string(rank) 
        | Diamonds =>  "Diamonds "^rank_to_string(rank)  
        | Hearts => "Hearts "^ rank_to_string(rank) 
        | Spades => "Spades "^rank_to_string(rank) 
  end
  in
    case x of
        Discard m =>"Discard "^cardToString(m)
        |Draw => "Draw "
  end
end

