module CNF where

-- | Syntax
data CNF = CNF { comment :: String       -- ^ A Comment
               , vars :: Integer         -- ^ Number of Variables
               , clauses :: [[Integer]]  -- ^ Clauses 
               }


data SAT = SAT { sComment :: String            -- ^ A Comment
               , sVars :: Integer              -- ^ Number of Variables
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

-- | affix a space to anything that can be shown
affixSp :: (Show a) => a -> String
affixSp = (++ " ") . show

-- | Given list of anything that can be shown, pretty format it
format :: (Show a) => [a] -> String
format [x] = show x ++ "\n"
format xs = mconcat $ hed : mid ++ [lst, "\n"]
  where hed = affixSp $ head xs
        mid = fmap affixSp . init . tail $ xs
        lst = (show $ last xs) ++ " 0" -- Ending in a 0 required by CNF form


-- | Show typeclasses for CNF and SAT formats
instance Show CNF where
  show CNF{comment, vars, clauses} =
    mconcat [ smtComment comment
            , "p cnf " -- required concrete syntax
            , affixSp vars
            , affixSp . toInteger $ length clauses
            , "\n"     -- end problem line
            , mconcat $ format <$> clauses
            ]

instance Show a => Show (Formula a) where
  show (Neg e) = "-" ++ show e
  show (Lit e) = show e
  show (And es) = mconcat ["*(", init $ format es, ")"]
  show (Or es)  = mconcat ["+(", init $ format es, ")"]
  show Nil     = ""

instance Show SAT where
  show SAT{sComment, sVars, formula} = 
    mconcat [ smtComment sComment
            , "p sat"
            , affixSp sVars
            , "\n" -- end of problem line
            , concatMap show formula 
            ]

-- | Examples
x :: CNF
x = CNF { comment = "I'm a comment"
        , vars = 2
        , clauses = [ [1, 2]
                    , [-1, 2]
                    ]
        }

y :: SAT
y = SAT { sComment = "Im a comment"
        , sVars = 4
        , formula = [(And
                      [ Or [Lit 1, Lit 3, Neg $ Lit 4]
                      , Or [Lit 4]
                      , Or [Lit 2, Lit 3]
                      ])
                    ]
        }
