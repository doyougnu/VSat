module CNF where

import qualified Data.Set as S
import Debug.Trace

-- | Syntax
data CNF = CNF { comment :: String       -- ^ A Comment
               , vars :: S.Set Integer   -- ^ Unique Variables
               , clauses :: [[Integer]]  -- ^ Clauses
               }


data SAT = SAT { sComment :: String            -- ^ A Comment
               , sVars :: S.Set Integer        -- ^ Unique Variables
               , formula :: [Formula Integer]  -- ^ One or more Formulas
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

-- | An empty CNF
emptyCNF :: CNF
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
instance Show CNF where
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
            , "p sat"
            , affixSp sVars
            , "\n" -- end of problem line
            , concatMap show formula
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

-- | Examples
x :: CNF
x = CNF { comment = "I'm a comment"
        , vars = S.fromList [1, 2]
        , clauses = [ [1, 2]
                    , [-1, 2]
                    ]
        }

y :: SAT
y = SAT { sComment = "Im a comment"
        , sVars = S.fromList [1..4]
        , formula = [(And
                      [ Or [Lit 1, Lit 3, Neg $ Lit 4]
                      , Or [Lit 4]
                      , Or [Lit 2, Lit 3]
                      ])
                    ]
        }
