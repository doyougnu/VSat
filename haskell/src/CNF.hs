module CNF where

import qualified Data.Set as S

import TagTree
import Utils

-- | Syntax
data CNF a = CNF { comment :: String        -- ^ A Comment
                 , vars :: S.Set Integer    -- ^ Unique Variables
                 , clauses :: [[a Integer]] -- ^ Clauses
                 }

-- | An empty CNF
emptyCNF :: CNF a
emptyCNF = CNF { comment = ""
               , vars = S.empty
               , clauses = [[]]
               }

-- | Given a CNF generate the variable set from the clauses
toVars :: CNF V -> S.Set Integer
toVars = S.fromList . concatMap (fmap abs . concatMap getAllObjs) . clauses -- fix this later

toVars' :: CNF Plain -> S.Set Integer
toVars' cnf = S.fromList . concatMap (fmap $ abs . yank) $ clauses cnf -- fix this later
  where yank (Plain a) = a

instance (Show (a Integer)) => Show (CNF a) where
  show CNF{comment, vars, clauses} =
    mconcat [ smtComment comment
            , "p cnf " -- required concrete syntax
            , affixSp $ S.size vars
            , affixSp . toInteger $ length clauses
            , "\n" --end problem statement
            , mconcat $ (flip (++) " 0\n" . format) <$> clauses
            ]

-- | Monoid CNF and SAT are based on monoids, and are therefore monoids
instance Monoid (CNF b) where
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
plainEx1 :: CNF Plain
plainEx1 = CNF { comment = "I'm a comment"
               , vars = S.fromList [1, 2]
               , clauses = [ [plain 1, plain 2]
                           , [plain (-1), plain 2]
                           ]
               }

-- | Some Variational Examples
vEx1 :: CNF V
vEx1 = CNF { comment = "I'm a comment"
           , vars = toVars vEx1
           , clauses = [ [one 1, one 2]
                       , [chc "a" (one 3) (one (-1)), one 2]
                       ]
           }

vEx2 :: CNF V
vEx2 = CNF { comment = "This one has two choice expressions"
           , vars = toVars vEx2
           , clauses = [ [chc "a" (one 1) (one (-1)), one 2, one 3]
                       , [chc "b" (one 2) (one 3), one 1, one (-1)]
                       , one <$> [1, (-2), 3]
                       ]
           }

vEx3 :: CNF V
vEx3 = CNF { comment = "This one has two choice expressions, one nested"
           , vars = toVars vEx3
           , clauses = [ [ chc "a" (one 1) (one (-1))
                         , one 2
                         , one 3
                         ]
                       , [ chc "b" (one 2) (chc "c" (one 2) (one (-3)))
                         , one 1, one (-1)
                         ]
                       , one <$> [1, (-2), 3]
                       ]
           }

vEx4 :: CNF V
vEx4 = CNF { comment = "This one is not solvable!"
           , vars = toVars vEx4
           , clauses = [ [one 1]
                       , [one (-1)]
                       ]
           }

vEx5 :: CNF V
vEx5 = CNF { comment = "Unsatisfiable based on Choices"
           , vars = toVars vEx5
           , clauses = [ [ chc "z" (one 1) (one (-1))]
                       , [chc "zz" (one 1) (one (-1))]
                       ]
           }
