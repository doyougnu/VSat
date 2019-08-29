module Parser where

import qualified Data.Text                  as T
import           Data.Void
-- import           Data.Char (isAlpha)
-- import           Control.Monad (when)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr
import           Prelude hiding (LT, EQ, GT)

import           VProp.Core (atMost1)
import           VProp.Types

type Parser = Parsec Void T.Text

langParser :: Parser (ReadableProp T.Text)
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

dot :: Parser T.Text
dot = symbol "."

prefix :: Parser a -> Parser a -> Parser a
prefix f g = f >> g

prefixDot :: Parser T.Text -> Parser T.Text
prefixDot = prefix $ symbol "."

dotSymbol :: T.Text -> Parser T.Text
dotSymbol = prefixDot . symbol

dash :: Parser T.Text
dash = symbol "-"

angledBrackets :: Parser a -> Parser a
angledBrackets = between (symbol "<") (symbol ">")

reserved :: T.Text -> Parser ()
reserved str = lexeme $ string str >> notFollowedBy alphaNumChar

bExpr :: Parser (ReadableProp T.Text)
bExpr = makeExprParser bTerm bOperators

bTerm :: Parser (ReadableProp T.Text)
bTerm = (dbg "paren" (parens bExpr))
        <|> (dbg "Chc" choiceTerm)
        <|> (dbg "At" atMost1Expr)
        <|> LitB True <$ reserved "true"
        <|> LitB False <$ reserved "false"
        <|> dbg "R" rExpr
        <|> try (dbg "B" boolRef)


aTerm :: Parser (VIExpr T.Text T.Text)
aTerm = (parens aExpr)
        <|> arithRef
        <|> (LitI . I . fromIntegral <$> integer)

dimension :: Parser (Dim T.Text)
dimension = lexeme $ Dim . T.pack <$> many upperChar

choiceTerm :: Parser (ReadableProp T.Text)
choiceTerm = do dim <- dimension
                (l,r) <- do _ <- symbol "<"
                            l' <- try bExpr
                            _ <- comma
                            r' <- try bExpr
                            _ <- symbol ">"
                            return (l', r')
                return $ ChcB dim l r

-- TODO probably going to be a performance hit
boolRef_ :: Parser T.Text
boolRef_ = lexeme $
  do a <- lowerChar
     -- when (isAlpha a) $
     --   fail "variables must be lower case and only be alphabetical"
     rest <- boolRef_
     return $ T.cons a rest

boolRef :: Parser (ReadableProp T.Text)
boolRef = lexeme $ RefB . T.pack <$> many lowerChar

arithRef :: Parser (VIExpr T.Text T.Text)
arithRef = lexeme $ (Ref RefD . T.pack) <$> many lowerChar

aVariable :: Parser T.Text
aVariable = mconcat <$> sepBy1 (T.pack <$> many lowerChar) dash

atMost1Expr :: Parser (ReadableProp T.Text)
atMost1Expr = do reserved "oneonly"
                 features <- brackets (sepBy1 boolRef comma)
                 return $! atMost1 features


bOperators :: [[Operator Parser (ReadableProp a)]]
bOperators =
  [ [ Prefix (OpB Not <$ reserved "not") ]
  ,
    [ InfixL (OpBB And    <$ reserved "and")
    , InfixL (OpBB Or     <$ reserved "or")
    , InfixR (OpBB Impl   <$ reserved "impl")
    , InfixN (OpBB BiImpl <$ reserved "iff")
    ]
  ]

aOperators :: [[Operator Parser (VIExpr a b)]]
aOperators =
  [ [ Prefix (OpI Neg <$ dotSymbol "-")]
  , [ InfixL (OpII Mult <$ dotSymbol "*")
    , InfixL (OpII Div <$  dotSymbol "/")
    , InfixL (OpII Add <$  dotSymbol "+")
    , InfixL (OpII Sub <$  dotSymbol "-")
    , InfixL (OpII Mod <$  dotSymbol "%")
    ]
  ]

relation :: Parser NN_B
relation = pure EQ <* symbol "="
           <|> pure LT  <* dotSymbol "<"
           <|> pure GTE <* dotSymbol "<="
           <|> pure LTE <* dotSymbol ">="
           <|> pure GT  <* dotSymbol ">"
           <|> pure NEQ <* dotSymbol "!="


rExpr :: Parser (ReadableProp T.Text)
rExpr = do
  a <- aTerm
  op <- relation
  b <- aTerm
  return (OpIB op a b)


aExpr :: Parser (VIExpr T.Text T.Text)
aExpr = makeExprParser aTerm aOperators
