module Prop where

data P a = P a
            | Neg (P a)
            | And (P a) (P a)
            | Or (P a) (P a)
            | Impl (P a) (P a)

toCNF :: (Integral a) => P a -> CNF
toCNF (Neg a) = negate
