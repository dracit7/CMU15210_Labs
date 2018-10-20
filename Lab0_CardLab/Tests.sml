structure Tests =
struct
open CardType
  val tests =  [
    ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15),
    ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42),
    ([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42)
  ]

end
