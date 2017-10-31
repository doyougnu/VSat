module SAT where

import qualified Data.Set as S

import Utils
import TagTree

data SAT a = SAT { sComment :: String                -- ^ A Comment
               , sVars :: S.Set Integer            -- ^ Unique Variables
               , formula :: [Formula (a Integer)]  -- ^ One or more Formula
               }

data Formula a = Neg (Formula a)  -- ^ a negation
               | Lit a            -- ^ a literal term
               | And [Formula a]  -- ^ a conjunction
               | Or [Formula a]   -- ^ a disjunction

-- | An empty SAT
emptySAT :: SAT a
emptySAT = SAT { sComment = ""
               , sVars = S.empty
               , formula = []
               }

-- | Typeclass instances
instance Show a => Show (Formula a) where
  show (Neg e) = "-" ++ show e
  show (Lit e) = show e
  show (And es) = mconcat ["*(", format es, ")"]
  show (Or es)  = mconcat ["+(", format es, ")"]

instance (Show (a Integer)) => Show (SAT a) where
  show SAT{sComment, sVars, formula} =
    mconcat [ smtComment sComment
            , "p sat "
            , affixSp $ S.size sVars
            , "\n" -- end of problem line
            , concatMap show formula
            ]

instance Monoid (SAT a) where
  mempty = emptySAT
  mappend
    SAT{sComment=lcomment, sVars=lvars, formula=lformula}
    SAT{sComment=rcomment, sVars=rvars, formula=rformula} =
    SAT { sComment = lcomment `mappend` rcomment
        , sVars = lvars `mappend` rvars
        , formula = pure . And $ lformula `mappend` rformula
        }
  mconcat = Prelude.foldr1 mappend


-- | Sat examples
plainEx2 :: SAT V
plainEx2 = SAT { sComment = "Im a comment"
               , sVars = S.fromList [1..4]
               , formula = [ And
                             [ Or [Lit (one 1), Lit (one 3), Neg $ Lit (one 4)]
                             , Or [Lit (one 4)]
                             , Or [Lit (one 2), Lit (one 3)]
                             ]
                           ]
               }
