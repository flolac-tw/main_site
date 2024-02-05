type List a = [a]

 --- Listifiable t holds if t is something that can be
 --- converted to a list.

class Listifiable t where
  listify :: t a -> List a

data Zero a = Unit
newtype One a = This a

data ListP a = Single a | Cons a (ListP a)

instance Listifiable Zero where
  listify Unit = []

instance Listifiable One where
  listify (This x) = [x]

instance Listifiable ListP where
  listify (Single x)  = [x]
  listify (Cons x xs) = x : listify xs

data ETree a = Tip a | Bin (ETree a) (ETree a)

data ITree a = Null | Node a (ITree a) (ITree a)

instance Listifiable ETree where
  listify (Tip x)   = [x]
  listify (Bin t u) = listify t ++ listify u

instance Listifiable ITree where
  listify Null         = []
  listify (Node a t u) = a : listify t ++ listify u

summation :: Listifiable t => t Int -> Int
summation = sum . listify

size :: Listifiable t => t a -> Int
size = length . listify

  -- Sample t holds if t is a type that can return
  -- a "sample element".

class Listifiable t => Sample t where
  sample :: t a -> a

instance Sample One where
  sample (This x) = x

instance Sample ListP where
  sample (Single x)  = x
  sample (Cons x xs) = x

instance Sample ETree where
  sample (Tip x)   = x
  sample (Bin t u) = sample t
