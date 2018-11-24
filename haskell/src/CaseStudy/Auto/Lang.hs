module CaseStudy.Auto.Lang where

data AutoLang a = AutoLit Bool
                | AutoRef a
                | Ctx RBOp (ALang a) (AutoLang a)
                | AutoNot (AutoLang a)
                | BBinary BOp (AutoLang a) (AutoLang a)
                | RBinary RBOp (ALang a) (ALang a)
                deriving (Show)

data BOp = And | Or | Impl | Eqv | Xor deriving Show
data RBOp = GRT | GRTE | EQL | LST | LSTE  | NEQL deriving Show

data ALang a = ALit Integer
             | AVar a
             | Neg (ALang a)
             | ABinary AOp (ALang a) (ALang a)
             deriving Show

data AOp = Add | Subtract | Multiply | Divide | Modulus deriving Show
