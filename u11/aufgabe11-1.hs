-- a)
type Money = Int
type Account = (Money, Money)

-- b)
withdraw :: Money -> Account -> Maybe Account
withdraw n (debit, credit) | n <= 0 && debit + credit + n >= 0 = Just (debit + n, credit)
                           | otherwise = Nothing

deposit :: Money -> Account -> Maybe Account
deposit n (debit, credit) | n >= 0 = Just (debit, credit + n)
                          | otherwise = Nothing

-- c)
meinTag :: Maybe Account
meinTag = do
    start <- Just (0, 0)
    t1 <- deposit 99 start
    t2 <- withdraw (-2) t1
    t3 <- withdraw (-15) t2
    t4 <- deposit 19 t3
    withdraw (-80) t4

meinTag' :: Maybe Account
meinTag' = do
    start <- Just (0, 0)
    t1 <- deposit 99 start
    t2 <- withdraw (-2) t1
    t3 <- withdraw (-150) t2
    t4 <- deposit 19 t3
    withdraw (-80) t4

-- d), e)
type Balance = Money

accountState :: Account -> Maybe Balance
accountState (debit, credit) = Just (debit + credit)

meinTag'' :: Maybe Balance
meinTag'' = 
    Just (0, 0) 
      >>= deposit 99
      >>= withdraw (-2)
      >>= withdraw (-15)
      >>= deposit 19
      >>= withdraw (-80)
      >>= accountState