type List a = [a]

 --- Listifiable t holds if t is something that can be
 --- converted to a list.

class Listifiable t where
  listify :: t a -> List a

data Zero a = Unit
newtype One a = This a

data ListP a = Single a | Cons a (ListP a)

instance Listifiable Zero where
  listify Unit = undefined

instance Listifiable One where
  listify (This a) = undefined

instance Listifiable ListP where
  listify = undefined

data ETree a = Tip a | Bin (Tree a) (Tree a)

data ITree a = Null | Node a (Tree a) (Tree a)

instance Listifiable ETree where
  listify = undefined

summation :: Listifiable t => t Int -> Int
summation = undefined

size :: Listifiable t => t a -> Int
size = undefined

  -- Sample t holds if t is a type that can return
  -- a "sample element".

class Listifiable t => Sample t where
  sample :: t a -> a

instance Sample One where
  sample (This x) = undefined

instance Sample ListP where
  sample = undefined

instance Sample ITree where
  sample = undefined
