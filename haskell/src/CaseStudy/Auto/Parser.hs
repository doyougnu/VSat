module CaseStudy.Auto.Parser where

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

import CaseStudy.Auto.Lang

type Parser = Parsec Void T.Text

langParser :: Parser (AutoLang T.Text)
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

bExpr :: Parser (AutoLang T.Text)
bExpr = makeExprParser bTerm bOperators

bTerm :: Parser (AutoLang T.Text)
bTerm =  (parens bExpr)
         <|> (try contextRef)
         <|> boolRef
         <|> rExpr
         <|> (AutoLit True <$ reserved "true")
         <|> (AutoLit False <$ reserved "false")

aTerm :: Parser (ALang T.Text)
aTerm = parens aExpr
        <|> aContextRef
        <|> ALit <$> integer
        <|> arithRef


aContextRef :: Parser (ALang T.Text)
aContextRef = do reserved "context"
                 _ <- brackets $ do
                   _ <- symbol "_"
                   reserved "evolution-context"
                 return . ACtx $ AVar "replaceMe!"

contextRef_ :: Parser (AutoLang a -> AutoLang a)
contextRef_ = do _ <- aContextRef
                 op <- relation
                 rhs <- integer
                 return $ Ctx op (ALit rhs)

contextRef :: Parser (AutoLang T.Text)
contextRef = do f <- parens contextRef_
                reserved "impl"
                rest <- bTerm
                return $ f rest

boolRef :: Parser (AutoLang T.Text)
boolRef = do reserved "feature"
             uuid <- brackets $ do
               _ <- anyChar
               aVariable
             return . AutoRef $ uuid

arithRef :: Parser (ALang T.Text)
arithRef = lexeme $ do reserved "feature"
                       uuid <- brackets $ do
                         _ <- symbol "_"
                         aVariable
                       return $ AVar uuid

aVariable :: Parser T.Text
aVariable = do a <- T.pack <$> many alphaNumChar
               f <- dash
               b <- T.pack <$> many alphaNumChar
               g <- dash
               c <- T.pack <$> many alphaNumChar
               h <- dash
               d <- T.pack <$> many alphaNumChar
               i <- dash
               e <- T.pack <$> many alphaNumChar
               return . mconcat $ [a,f,b,g,c,h,d,i,e]

bOperators :: [[Operator Parser (AutoLang a)]]
bOperators =
  [ [ Prefix (AutoNot <$ reserved "not") ]
  ,
    [ InfixL (BBinary And <$ reserved "and")
    , InfixL (BBinary Or <$ reserved "or")
    , InfixR (BBinary Impl <$ reserved "impl")
    , InfixN (BBinary Eqv <$ reserved "iff")
    , InfixN (BBinary Xor <$ reserved "oneonly") -- not sure what Xor is for this lang
    ]
  ]

relation :: Parser RBOp
relation = pure EQL <* symbol "="
           <|> pure LST  <* symbol "<"
           <|> pure GRTE <* symbol "<="
           <|> pure LSTE <* symbol ">="
           <|> pure GRT  <* symbol ">"
           <|> pure NEQL <* symbol "!="


aRelation :: Parser AOp
aRelation = pure Add <* symbol "+"
           <|> pure Subtract <* symbol "-"
           <|> pure Multiply <* symbol "*"
           <|> pure Divide <* symbol "/"
           <|> pure Modulus <* symbol "%"

rExpr :: Parser (AutoLang T.Text)
rExpr = do
  a <- (lexeme aTerm)
  op <- relation
  b <- (lexeme aTerm)
  return (RBinary op a b)


aExpr :: Parser (ALang T.Text)
aExpr = makeExprParser aTerm aOperators
