structure CardProblem =
struct
open CardType
exception NYI

fun Card_Color(Card:card):color = 
	case Card of
		(Hearts,_) => Red
		| (Diamonds,_) => Red
		| _ => Black

fun Card_Value(Card:card):int = 
	case Card of
		(_,Num(i)) => i
		| (_,Ace) => 11
		| _ => 10

fun Remove_Card([],Target,Exc) = raise Exc
	| Remove_Card(TopCard::CardList,Target,Exc) =
			if TopCard = Target then CardList
			else TopCard::Remove_Card(CardList,Target,Exc)

local
	fun JudgeColor([],_) = true
		| JudgeColor(TopCard::CardList,TargetColor) =
				if TargetColor = Card_Color TopCard then JudgeColor(CardList,TargetColor)
				else false
in
	fun All_Same_Color([]) = true
	| All_Same_Color(TopCard::CardList) =
			JudgeColor(CardList,Card_Color TopCard)
end
(* A pretty awkward implementation... *)

local
	fun Count(Sum,[]) = Sum
		| Count(Sum,TopCard::CardList) =
			Count(Sum + Card_Value(TopCard),CardList)
in
	fun Sum_Cards(CardList) = Count(0,CardList)
end
(* Tail recursion, as you wish. *)

local
	fun RawScore(CardList,Goal) =
		if Sum_Cards(CardList) > Goal then 3 * (Sum_Cards(CardList) - Goal)
		else Goal - Sum_Cards(CardList)
in
	fun Score(CardList,Goal) =
		if All_Same_Color CardList then RawScore(CardList,Goal) div 2
		else RawScore(CardList,Goal)
end

(* Why did you put a underscore there? Why did u do that? TAT *)
(* I thought it was a special grammar and spent two days on finding
how I can get those args if the param is a underscore QAQ *)
(* Can't you guys just add a quote here TAT *)
local
	fun Solution (nil,_,Goal,HeldCards) =
				Score(HeldCards,Goal)
		| Solution (_,nil,Goal,HeldCards) =
				Score(HeldCards,Goal)
		| Solution (TopCard::CardList,NextMove::MoveList,Goal,HeldCards) =
				case NextMove of
					Discard(Target) =>
						Solution(TopCard::CardList,MoveList,Goal,Remove_Card(HeldCards,Target,IllegalMove))
				| Draw => if Sum_Cards(TopCard::HeldCards) > Goal then
										Score(TopCard::HeldCards,Goal)
									else Solution(CardList,MoveList,Goal,TopCard::HeldCards)
in
	fun officiate(CardList,MoveList,Goal) =
		Solution(CardList,MoveList,Goal,nil)		
end
(* It's not hard at all, right? *)
(* The point is I didn't know that I should replace that underscore
with params by myself !!! I thought it was another feature of sml !!
and I wasted my time searching for details of this feature !! *)

end