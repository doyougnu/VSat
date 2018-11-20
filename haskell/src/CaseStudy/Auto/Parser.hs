module CaseStudy.Auto.Parser where

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

import VProp.Types
import VProp.Core()
import Prelude hiding (EQ, LT, GT)

type Parser = Parsec Void T.Text

langParser :: Parser (VProp T.Text T.Text)
langParser = between sc eof bTerm

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

andExpr :: Parser (VProp T.Text T.Text)
andExpr = Opn And <$> sepBy1 bTerm (reserved "and")

orExpr :: Parser (VProp T.Text T.Text)
orExpr = Opn Or <$> sepBy1 bTerm (reserved "or")

reserved :: T.Text -> Parser ()
reserved str = lexeme $ string str >> notFollowedBy alphaNumChar

aOperators :: [[Operator Parser (VIExpr a)]]
aOperators =
  [ [ Prefix (OpI Neg   <$ symbol "-")]
  , [ InfixL (OpII Mult <$ symbol "*")
    , InfixL (OpII Div  <$ symbol "/")
    , InfixL (OpII Add  <$ symbol "+")
    , InfixL (OpII Sub  <$ symbol "-")
    , InfixL (OpII Mod  <$ symbol "%")
    ]
  ]

bExpr :: Parser (VProp T.Text T.Text)
bExpr = makeExprParser bTerm bOperators

bTerm :: Parser (VProp T.Text T.Text)
bTerm = parens bExpr
        <|> (LitB True <$ reserved "true")
        <|> (LitB False <$ reserved "false")
        <|> rExpr
        <|> andExpr
        <|> orExpr
        <|> boolRef

aTerm :: Parser (VIExpr T.Text)
aTerm = parens aExpr
        <|> LitI . I <$> integer
        <|> LitI . D <$> L.float
        <|> arithRef

boolRef :: Parser (VProp T.Text b)
boolRef = do reserved "feature"
             uuid <- brackets $ do
               _ <- anyChar
               aVariable
             return . RefB $ uuid

arithRef :: Parser (VIExpr T.Text)
arithRef = do reserved "feature"
              uuid <- brackets $ do
                _ <- symbol "_"
                aVariable
              return $ Ref RefI uuid

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

bOperators :: [[Operator Parser (VProp a b)]]
bOperators =
  [ [ Prefix (OpB Not <$ reserved "not") ]
  ,
    [ InfixL (OpBB Impl <$ reserved "impl")
    , InfixL (OpBB BiImpl <$ reserved "iff")
    , InfixL (OpBB XOr <$ reserved "???") -- not sure what Xor is for this lang
    ]
  ]

relation :: Parser NN_B
relation = pure EQ <* symbol "="
           <|> pure LT  <* symbol "<"
           <|> pure GTE <* symbol "<="
           <|> pure LTE <* symbol ">="
           <|> pure GT  <* symbol ">"
           <|> pure NEQ <* symbol "!="

rExpr :: Parser (VProp a T.Text)
rExpr = do
  a <- aTerm
  op <- relation
  b <- aTerm
  return (OpIB op a b)


aExpr :: Parser (VIExpr T.Text)
aExpr = makeExprParser aTerm aOperators
