module CaseStudy.Auto.Parser where

import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr

import           CaseStudy.Auto.Lang

type Parser = Parsec Void T.Text

langParser :: Parser (AutoLang T.Text T.Text)
langParser = between sc eof bExpr

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

brackets :: Parser a -> Parser a
brackets  = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

underscore :: Parser ()
underscore = symbol "_" >> return ()

comma :: Parser T.Text
comma = symbol ","

dash :: Parser T.Text
dash = symbol "-"

reserved :: T.Text -> Parser ()
reserved str = lexeme $ string str >> notFollowedBy alphaNumChar

bExpr :: Parser (AutoLang T.Text T.Text)
bExpr = makeExprParser bTerm bOperators

bTerm :: Parser (AutoLang T.Text T.Text)
bTerm =  (parens bExpr)
         <|> (try contextRef)
         <|> boolRef
         <|> rExpr
         <|> atMost1Expr
         <|> (AutoLit True <$ reserved "true")
         <|> (AutoLit False <$ reserved "false")


aTerm :: Parser (ALang T.Text)
aTerm = (parens aExpr)
        <|> (try aContextRef)
        <|> (try arithRef)
        <|> (ALit <$> integer)

aContextRef :: Parser (ALang T.Text)
aContextRef = do reserved "context"
                 _ <- brackets $ do
                   _ <- symbol "_"
                   reserved "evolution-context"
                 return . ACtx $ AVar "evo_ctx"

contextRef_ :: Parser (AutoLang a a -> AutoLang a a)
contextRef_ = do _ <- aContextRef
                 op <- relation
                 rhs <- integer
                 return $ Ctx op (ALit rhs)

contextRef :: Parser (AutoLang T.Text T.Text)
contextRef = do f <- parens contextRef_
                reserved "impl"
                rest <- bTerm
                return $ f rest

boolRef :: Parser (AutoLang T.Text T.Text)
boolRef = do reserved "feature"
             uuid <- brackets $ do
               _ <- underscore
               aVariable
             return . AutoRef $ uuid

arithRef :: Parser (ALang T.Text)
arithRef = do reserved "feature"
              uuid <- brackets $ do
                _ <- symbol "_"
                aVariable
              return $ AVar uuid

aVariable :: Parser T.Text
aVariable = mconcat <$> sepBy1 (T.pack <$> many alphaNumChar) dash

atMost1Expr :: Parser (AutoLang T.Text T.Text)
atMost1Expr = do reserved "oneonly"
                 features <- brackets (sepBy1 boolRef comma)
                 return $! atMost1 features


bOperators :: [[Operator Parser (AutoLang a a)]]
bOperators =
  [ [ Prefix (AutoNot <$ reserved "not") ]
  ,
    [ InfixL (BBinary And <$ reserved "and")
    , InfixL (BBinary Or <$ reserved "or")
    , InfixR (BBinary Impl <$ reserved "impl")
    , InfixN (BBinary Eqv <$ reserved "iff")
    ]
  ]

aOperators :: [[Operator Parser (ALang a)]]
aOperators =
  [ [ Prefix (Neg <$ symbol "-")]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide <$ symbol "/")
    , InfixL (ABinary Add <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-")
    , InfixL (ABinary Modulus <$ symbol "%")
    ]
  ]

relation :: Parser RBOp
relation = pure EQL <* symbol "="
           <|> pure LST  <* symbol "<"
           <|> pure GRTE <* symbol "<="
           <|> pure LSTE <* symbol ">="
           <|> pure GRT  <* symbol ">"
           <|> pure NEQL <* symbol "!="


rExpr :: Parser (AutoLang T.Text T.Text)
rExpr = do
  a <- aTerm
  op <- relation
  b <- aTerm
  return (RBinary op a b)


aExpr :: Parser (ALang T.Text)
aExpr = makeExprParser aTerm aOperators
