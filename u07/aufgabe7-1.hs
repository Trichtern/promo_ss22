data Suit = Club | Heart | Spade | Diamond deriving (Enum, Show, Eq)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Show, Eq, Ord)
data Card = Card Suit Value

instance Show Card where
    show (Card suit value) = show value ++ " of " ++ show suit ++ "s"
 
instance Ord Card where
    compare (Card Heart v1) (Card Heart v2) = compare v1 v2
    compare (Card Heart _) (Card _ _) = GT
    compare (Card _ _) (Card Heart _) = LT
    compare (Card _ v1) (Card _ v2) = compare v1 v2

instance Eq Card where
    a == b = compare a b == EQ

deck :: [Card]
deck = [Card suit value | suit <- [Club ..], value <- [Two ..]]