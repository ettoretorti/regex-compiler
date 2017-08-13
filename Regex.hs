{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Regex (
    Regex(..),
    rep,
    comp,
    seq,
    seqM,
    alt,
    altM,
    inter,
    interM,
    rmap,
    normalize,
    derivative,
    nullable,
    matches,
    toStr,
) where
import Prelude hiding (seq)
import Data.List (sort)

data Regex a where
    Null :: (Eq a, Ord a) => Regex a
    Empty :: (Eq a, Ord a) => Regex a
    Term :: (Eq a, Ord a) => a -> Regex a
    Rep :: (Eq a, Ord a) => Regex a -> Regex a
    Comp :: (Eq a, Ord a) => Regex a -> Regex a
    Seq :: (Eq a, Ord a) => Regex a -> Regex a -> Regex a
    Alt :: (Eq a, Ord a) => Regex a -> Regex a -> Regex a
    Inter :: (Eq a, Ord a) => Regex a -> Regex a -> Regex a
deriving instance Show a => Show (Regex a)
deriving instance (Ord a, Eq a, Read a) => Read (Regex a)
deriving instance Eq a => Eq (Regex a)
deriving instance Ord a => Ord (Regex a)

-- utils
dedupe :: (Eq a) => [a] -> [a]
dedupe xs = foldr (\x y -> if null y then [x] else if x == head y then y else (x:y)) [] xs

sortUniq :: (Eq a, Ord a) => [a] -> [a]
sortUniq = dedupe . sort

uniqFoldr1 :: (Eq a, Ord a) => (a -> a -> a) -> [a] -> a
uniqFoldr1 f xs = case sortUniq xs of
                   [x] -> x
                   xs  -> foldr1 f xs

-- real stuff
rep :: (Eq a, Ord a) => Regex a -> Regex a
rep Null = Empty
rep Empty = Empty
rep (Rep a) = Rep a
rep x = Rep x

comp :: (Eq a, Ord a) => Regex a -> Regex a
comp (Comp x) = x
comp x = Comp x

seq :: (Eq a, Ord a) => Regex a -> Regex a -> Regex a
seq Null _ = Null
seq _ Null = Null
seq Empty a = a
seq a Empty = a
seq (Seq a b) c = seq a (seq b c)
seq a b = Seq a b

seqM :: (Eq a, Ord a) => [Regex a] -> Regex a
seqM rgxs = foldr seq Empty rgxs

alt :: (Eq a, Ord a) => Regex a -> Regex a -> Regex a
alt Null a = a
alt a Null = a
alt (Comp Null) _ = Comp Null
alt _ (Comp Null) = Comp Null
alt (Alt a b) (Alt c d) = uniqFoldr1 Alt [a,b,c,d]
alt (Alt a b) c = alt a (Alt b c)
alt a (Alt b c) = uniqFoldr1 Alt [a,b,c]
alt a b = case compare a b of
            GT -> Alt b a
            EQ -> a
            LT -> Alt a b

altM :: (Eq a, Ord a) => [Regex a] -> Regex a
altM rgxs = foldr alt Null $ sortUniq rgxs

inter :: (Eq a, Ord a) => Regex a -> Regex a -> Regex a
inter Null _ = Null
inter _ Null = Null
inter Empty (Term _) = Null
inter (Term _) Empty = Null
inter (Comp Null) a = a
inter a (Comp Null) = a
inter (Term a) (Term b) = if a == b then Term a else Null
inter (Inter a b) (Inter c d) = uniqFoldr1 Inter [a,b,c,d]
inter (Inter a b) c = inter a (Inter b c)
inter a (Inter b c) = uniqFoldr1 Inter [a,b,c]
inter a b = case compare a b of
          GT -> Inter b a
          EQ -> a
          LT -> Inter a b

interM :: (Eq a, Ord a) => [Regex a] -> Regex a
interM rgxs = foldr inter (Comp Null) $ sortUniq rgxs

-- in case smart constructors weren't used
normalize :: (Eq a, Ord a) => Regex a -> Regex a
normalize Null = Null
normalize Empty = Empty
normalize (Term x) = Term x
normalize (Rep x) = rep $ normalize x
normalize (Comp x) = comp  $ normalize x
normalize (Seq x y) = seq (normalize x) (normalize y)
normalize (Alt x y) = alt (normalize x) (normalize y)
normalize (Inter x y) = inter (normalize x) (normalize y)

rmap :: (Eq a, Ord a, Eq b, Ord b) => (a -> b) -> Regex a -> Regex b
rmap _ Null = Null
rmap _ Empty = Empty
rmap f (Term x) = Term $ f x
rmap f (Rep x) = Rep $ rmap f x
rmap f (Comp x) = Comp $ rmap f x
rmap f (Seq x y) = Seq (rmap f x) (rmap f y)
rmap f (Alt x y) = Alt (rmap f x) (rmap f y)
rmap f (Inter x y) = Inter (rmap f x) (rmap f y)

nullable :: Regex a -> Bool
nullable Empty = True
nullable Null = False
nullable (Term _) = False
nullable (Alt a b) = nullable a || nullable b
nullable (Seq a b) = nullable a && nullable b
nullable (Inter a b) = nullable a && nullable b
nullable (Rep _) = True
nullable (Comp x) = not $ nullable x

v :: (Eq a, Ord a) => Regex a -> Regex a
v r = if nullable r then Empty else Null

dr :: (Eq a, Ord a) => a -> Regex a -> Regex a
dr _ Null = Null
dr _ Empty = Null
dr x (Term y) = if x == y then Empty else Null
dr x (Seq a b) = alt (seq (dr x a) b) (seq (v a) (dr x b))
dr x (Alt a b) = alt (dr x a) (dr x b)
dr x (Inter a b) = inter (dr x a) (dr x b)
dr x (Rep a) = seq (dr x a) (rep a)
dr x (Comp a) = comp $ dr x a

-- better name for export
derivative :: (Eq a, Ord a) => a -> Regex a -> Regex a
derivative = dr

matches :: (Eq a, Ord a) => [a] -> Regex a -> Bool
matches _ Null = False
matches [] a = nullable a
matches (x:xs) a = matches xs $ dr x a

toStr :: (Show a) => Regex a -> String
toStr Null  = "$NULL$"
toStr Empty = ""
toStr (Term a) = show a
toStr (Rep (Seq a b)) = "(" ++ toStr (Seq a b) ++ ")*"
toStr (Rep a) = toStr a ++ "*"
toStr (Comp (Seq a b)) = "!(" ++ toStr (Seq a b) ++ ")"
toStr (Comp a) = "!" ++ toStr a
toStr (Seq a b) = toStr a ++ toStr b
toStr (Alt a b) = "(" ++ escape (Alt a b) ++ ")"
    where escape (Alt a (Alt b c)) = toStr a ++ "|" ++ escape (Alt b c)
          escape (Alt a b) = toStr a ++ "|" ++ toStr b
toStr (Inter a b) = "(" ++ escape (Inter a b) ++ ")"
    where escape (Inter a (Inter b c)) = toStr a ++ "&" ++ escape (Inter b c)
          escape (Inter a b) = toStr a ++ "&" ++ toStr b
