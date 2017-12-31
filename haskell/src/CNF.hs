module CNF where

import qualified Data.Set as S

import Utils

-- | Syntax
data CNF = CNF { comment :: String        -- ^ A Comment
               , vars :: S.Set Integer    -- ^ Unique Variables
               , clauses :: [[Integer]] -- ^ Clauses
               }

-- | An empty CNF
emptyCNF :: CNF
emptyCNF = CNF { comment = ""
               , vars = S.empty
               , clauses = [[]]
               }

-- | Given a CNF generate the variable set from the clauses
genVars :: CNF -> CNF
genVars CNF{comment=cs, vars=_, clauses=cls} = CNF { comment = cs
                                                    , vars    = genVars' cls
                                                    , clauses = cls
                                                    }
  where genVars' = S.fromList . concatMap (fmap abs)

instance Show CNF where
  show CNF{comment, vars, clauses} =
    mconcat [ smtComment comment
            , "p cnf " -- required concrete syntax
            , affixSp $ S.size vars
            , affixSp . toInteger $ length clauses
            , "\n" --end problem statement
            , mconcat $ (flip (++) " 0\n" . format) <$> clauses
            ]

-- | Monoid CNF and SAT are based on monoids, and are therefore monoids
instance Monoid CNF where
  mempty = emptyCNF
  mappend
    CNF{comment=lcomment, vars=lvars, clauses=lclauses}
    CNF{comment=rcomment, vars=rvars, clauses=rclauses} =
    CNF { comment = lcomment `mappend` rcomment
        , vars = lvars `mappend` rvars
        , clauses = lclauses `mappend` rclauses
        }
  mconcat = Prelude.foldr1 mappend

-- | Plain Examples
plainEx1 :: CNF
plainEx1 = CNF { comment = "I'm a comment"
               , vars = S.fromList [0]
               , clauses = [ [1, 2]
                           , [(-1), 2]
                           ]
               }
