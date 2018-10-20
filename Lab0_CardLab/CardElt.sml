structure CardElt : ELEMENT =
struct
open CardType
exception NYI
type t = card

  val default = (Clubs,Ace)
  val equal = op=
  fun compare (x,y)= 
  let
  fun card_value rnk =
    case rnk of
        (_,Num n) => n
       |(_,Ace) => 11
       |_ => 10
       in
  Int.compare (card_value x,card_value y)
  end
  fun hash i = raise NYI
  fun toString (suit,rank) =
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
end

