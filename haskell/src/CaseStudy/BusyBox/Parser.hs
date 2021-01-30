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
import           Data.Char (isAlphaNum)

import           VProp.Core (atMost1)
import           VProp.Types

type Parser = Parsec Void T.Text

langParser :: Parser (ReadableProp T.Text)
{-# INLINE langParser #-}
langParser = between sc eof bExpr

sc :: Parser ()
{-# INLINE sc #-}
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
{-# INLINE lexeme #-}
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
{-# INLINE symbol #-}
symbol = L.symbol sc

parens :: Parser a -> Parser a
{-# INLINE parens #-}
parens = between (symbol "(") (symbol ")")

underscore :: Parser ()
{-# INLINE underscore #-}
underscore = do _ <- symbol "_"; return ()

def :: Parser ()
{-# INLINE def #-}
def = reserved "def"

reserved :: T.Text -> Parser ()
{-# INLINE reserved #-}
reserved str = lexeme $ string str >> notFollowedBy alphaNumChar

bExpr :: Parser (ReadableProp T.Text)
{-# INLINE bExpr #-}
bExpr = makeExprParser bTerm bOperators

bTerm :: Parser (ReadableProp T.Text)
{-# INLINE bTerm #-}
bTerm = parens bExpr
        <|> boolRef_

-- TODO probably going to be a performance hit
boolRef_ :: Parser (ReadableProp T.Text)
{-# INLINE boolRef_ #-}
boolRef_ = do def; parens boolRef

boolRef :: Parser (ReadableProp T.Text)
{-# INLINE boolRef #-}
-- boolRef = RefB . mconcat <$> sepBy (T.pack <$> many alphaNumChar) underscore
boolRef = RefB . mconcat <$> sepBy (takeWhile1P Nothing isAlphaNum) underscore

bOperators :: [[Operator Parser (ReadableProp a)]]
{-# INLINE bOperators #-}
bOperators =
  [ [ Prefix (OpB Not <$ symbol "!") ]
  ,
    [ InfixL (OpBB And    <$ symbol "&")
    , InfixL (OpBB Or     <$ symbol "|")
    ]
  ]
