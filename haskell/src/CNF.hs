module CNF where

import qualified Data.Set as S

import TagTree

data Plain
data Variational

-- | Syntax
data CNF a b = CNF { comment :: String        -- ^ A Comment
                   , vars :: S.Set Integer    -- ^ Unique Variables
                   , clauses :: [[b Integer]] -- ^ Clauses
                   }


data SAT = SAT { sComment :: String                -- ^ A Comment
               , sVars :: S.Set Integer            -- ^ Unique Variables
               , formula :: [Formula (V Integer)]  -- ^ One or more Formula
               }

data Formula a = Neg (Formula a)  -- ^ a negation
               | Lit a            -- ^ a literal term
               | And [Formula a]  -- ^ a conjunction
               | Or [Formula a]   -- ^ a disjunction
               | Nil              -- ^ Null terminator

-- | smart constructor for comments
smtComment :: (Show a) => a -> String
smtComment stmt = mconcat [ "c "
                          , show stmt
                          , "\n"
                          ]

-- | smart constructor for Variables
smtVars :: (Integral a) => [a] -> S.Set Integer
smtVars = S.fromList . fmap toInteger

-- | An empty CNF
emptyCNF :: CNF a b
emptyCNF = CNF { comment = ""
               , vars = S.empty
               , clauses = [[]]
               }

-- | An empty SAT
emptySAT :: SAT
emptySAT = SAT { sComment = ""
               , sVars = S.empty
               , formula = []
               }

-- | Given a CNF generate the variable set from the clauses
toVars :: CNF a V -> S.Set Integer
toVars = S.fromList . concatMap (fmap abs . concatMap getAllObjs) . clauses -- fix this later

-- | affix a space to anything that can be shown
affixSp :: (Show a) => a -> String
affixSp = (++ " ") . show

-- | Given list of anything that can be shown, pretty format it
format :: (Show a) => [a] -> String
format [] = ""
format [x] = show x
format (x:xs) = mconcat $ hed : mid ++ [lst]
  where hed = affixSp x
        mid = fmap affixSp . init $ xs
        lst = (show $ last xs)

-- | Show typeclasses for CNF and SAT formats
instance (Show (b Integer)) => Show (CNF a b) where
  show CNF{comment, vars, clauses} =
    mconcat [ smtComment comment
            , "p cnf " -- required concrete syntax
            , affixSp $ S.size vars
            , affixSp . toInteger $ length clauses
            , "\n" --end problem statement
            , mconcat $ ((flip (++) " 0\n") . format) <$> clauses
            ]

instance Show a => Show (Formula a) where
  show (Neg e) = "-" ++ show e
  show (Lit e) = show e
  show (And es) = mconcat ["*(", format es, ")"]
  show (Or es)  = mconcat ["+(", format es, ")"]
  show Nil     = ""

instance Show SAT where
  show SAT{sComment, sVars, formula} =
    mconcat [ smtComment sComment
            , "p sat "
            , affixSp $ S.size sVars
            , "\n" -- end of problem line
            , concatMap show formula
            ]

-- | Monoid CNF and SAT are based on monoids, and are therefore monoids
instance Monoid (CNF a b) where
  mempty = emptyCNF
  mappend
    CNF{comment=lcomment, vars=lvars, clauses=lclauses}
    CNF{comment=rcomment, vars=rvars, clauses=rclauses} =
    CNF { comment = lcomment `mappend` rcomment
        , vars = lvars `mappend` rvars
        , clauses = lclauses `mappend` rclauses
        }
  mconcat xs = Prelude.foldr1 mappend xs

instance Monoid SAT where
  mempty = emptySAT
  mappend
    SAT{sComment=lcomment, sVars=lvars, formula=lformula}
    SAT{sComment=rcomment, sVars=rvars, formula=rformula} =
    SAT { sComment = lcomment `mappend` rcomment
        , sVars = lvars `mappend` rvars
        , formula = pure . And $ lformula `mappend` rformula
        }
  mconcat xs = Prelude.foldr1 mappend xs

-- | Plain Examples
plainEx1 :: CNF Plain V
plainEx1 = CNF { comment = "I'm a comment"
               , vars = S.fromList [1, 2]
               , clauses = [ [(one 1), (one 2)]
                           , [(one (-1)), (one 2)]
                           ]
               }

plainEx2 :: SAT
plainEx2 = SAT { sComment = "Im a comment"
               , sVars = S.fromList [1..4]
               , formula = [(And
                             [ Or [Lit (one 1), Lit (one 3), Neg $ Lit (one 4)]
                             , Or [Lit (one 4)]
                             , Or [Lit (one 2), Lit (one 3)]
                             ])
                           ]
               }

-- | Some Variational Examples
vEx1 :: CNF Variational V
vEx1 = CNF { comment = "I'm a comment"
           , vars = toVars vEx1
           , clauses = [ [(one 1), (one 2)]
                       , [(chc 3 (one 3) (one (-1))), (one 2)]
                       ]
           }

vEx2 :: CNF Variational V
vEx2 = CNF { comment = "This one has two choice expressions"
           , vars = toVars vEx2
           , clauses = [ [chc 1 (one 1) (one (-1)), one 2, one 3]
                       , [chc 2 (one 2) (one 3), one 1, one (-1)]
                       , one <$> [1, (-2), 3]
                       ]
           }

vEx3 :: CNF Variational V
vEx3 = CNF { comment = "This one has two choice expressions, one nested"
           , vars = toVars vEx3
           , clauses = [ [ chc 1 (one 1) (one (-1))
                         , one 2
                         , one 3
                         ]
                       , [ chc 2 (one 2) (chc 3 (one 2) (one (-3)))
                         , one 1, one (-1)
                         ]
                       , one <$> [1, (-2), 3]
                       ]
           }
