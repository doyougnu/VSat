module CaseStudy.BusyBox.Parser where

import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr
import           Prelude hiding (LT, EQ, GT)
import           Control.Monad (void)
import           Data.Functor ((<$))

import           VProp.Core (atMost1)
import           VProp.Types

type Parser = Parsec Void T.Text

langParser :: Parser (ReadableProp T.Text)
langParser = between sc eof bExpr

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

underscore :: Parser ()
underscore = void $ symbol "_"

def :: Parser ()
def = reserved "def"

reserved :: T.Text -> Parser ()
reserved str = lexeme $ string str >> notFollowedBy alphaNumChar

bExpr :: Parser (ReadableProp T.Text)
bExpr = makeExprParser bTerm bOperators

bTerm :: Parser (ReadableProp T.Text)
bTerm = parens bExpr
        <|> boolRef_

-- TODO probably going to be a performance hit
boolRef_ :: Parser (ReadableProp T.Text)
boolRef_ = do def; parens boolRef

boolRef :: Parser (ReadableProp T.Text)
boolRef = RefB . mconcat <$> sepBy (T.pack <$> many alphaNumChar) underscore

bOperators :: [[Operator Parser (ReadableProp a)]]
bOperators =
  [ [ Prefix (OpB Not <$ symbol "!") ]
  ,
    [ InfixL (OpBB And    <$ symbol "&")
    , InfixL (OpBB Or     <$ symbol "|")
    ]
  ]
