{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Test.SmallCheck
import Test.SmallCheck.Series

data NList1 a
  = Empty
  | ConsElem a (NList1 a)
  | ConsList (NList1 a) (NList1 a)
  deriving (Show, Eq, Generic)

data NList2 a
  = Elem a
  | List [NList2 a]
  deriving (Show, Generic)

instance Eq a => Eq (NList2 a) where
  (Elem a) == (Elem b) = a == b
  (List a) == (List b) = a == b
  a == b = convert a == convert b

instance Serial m a => Serial m (NList1 a)
instance Serial m a => Serial m (NList2 a)

test_list1 :: NList1 Integer
test_list1 = ConsElem 2 $ ConsList (ConsElem 2 $ ConsElem 3 $ Empty)
  $ ConsElem 4 Empty

test_list2 :: NList2 Integer
test_list2 = List [Elem 2, List [Elem 2, Elem 3], Elem 4]

from1To2 :: NList1 a -> NList2 a
from1To2 Empty = List []
from1To2 (ConsElem a b) = let List b' = from1To2 b in List $ Elem a : b'
from1To2 (ConsList a b) =
  let
    List a' = from1To2 a
    List b' = from1To2 b
  in List $ List a' : b'

from2To1' :: NList2 a -> NList1 a
from2To1' (List []) = Empty
from2To1' (List (Elem a:xs)) = ConsElem a (from2To1 $ List xs)
from2To1' (List (List a:xs)) = ConsList (from2To1 $ List a) (from2To1 $ List xs)
from2To1 = from2To1' . convert

convert (Elem x) = List [Elem x]
convert x = x

main = do
  let depth = 4
  putStrLn "1 -> 2 -> 1 =?= id"
  smallCheck depth $ \l -> from2To1 (from1To2 l) == (l :: NList1 ())
  putStrLn "2 -> 1 -> 2 =?= id"
  smallCheck depth $ \l -> from1To2 (from2To1 l) == (l :: NList2 ())
