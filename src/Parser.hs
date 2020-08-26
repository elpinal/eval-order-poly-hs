{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parse
  ) where

import Data.Functor
import Data.List.NonEmpty
import Data.Void

import Control.Monad.Combinators.Expr
import Data.Functor.Foldable (Fix(..))
import qualified Data.Text as T
import Text.Megaparsec hiding (parse, match)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import EvalOrder
import Surface
import qualified Term
import qualified Type

type Parser = Parsec Void T.Text

type Term = Term.Term Surface
type Type = Type.Type Surface

parse :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) Term
parse file input = runParser (sc *> expr <* eof) file input

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser ()
symbol = void . L.symbol sc

keywords :: [T.Text]
keywords =
  [ "fun"
  , "fix"
  , "match"
  , "with"
  , "end"
  , "fst"
  , "snd"
  , "inl"
  , "inr"
  , "V"
  , "N"
  ]

lowerIdent :: Parser [Char]
lowerIdent = lexeme $ (:) <$> lowerChar <*> many alphaNumChar

upperIdent :: Parser [Char]
upperIdent = lexeme $ (:) <$> upperChar <*> many alphaNumChar

keyword :: T.Text -> Parser ()
keyword k = lexeme $ do
  _ <- string k
  notFollowedBy alphaNumChar
  pure ()

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

fun :: Parser ()
fun = symbol "λ" <|> keyword "fun"

gen :: Parser ()
gen = symbol "Λ"

var :: Parser Term.Variable
var = try $ do
  s <- lowerIdent
  if T.pack s `elem` keywords
  then unexpected $ Tokens $ fromList s
  else return $ Term.Variable s

tvar :: Parser TypeVar
tvar = do
  symbol "'"
  s <- lowerIdent
  return $ TypeVar s

evar :: Parser EvalVar
evar = do
  symbol "'"
  s <- upperIdent
  return $ EvalVar s

term :: Parser Term
term = choice
  [ try $ parens $ return $ Fix Term.Unit
  , try $ parens expr
  , parens $ (Fix .) . Term.Pair <$> expr <*> (symbol "," *> expr)
  , Fix . Term.Var <$> var
  ]

aexpr :: Parser Term
aexpr = choice
  [ Fix . Term.Proj Term.Fst <$> (keyword "fst" *> term)
  , Fix . Term.Proj Term.Snd <$> (keyword "snd" *> term)
  , Fix . Term.Inj Term.Fst <$> (keyword "inl" *> term)
  , Fix . Term.Inj Term.Snd <$> (keyword "inr" *> term)
  , term
  ] >>= argPos

argPos :: Term -> Parser Term
argPos t1 = choice
  [ Fix . Term.App t1 <$> term >>= argPos
  , Fix . Term.Inst t1 <$> brackets typeParser >>= argPos
  , return t1
  ]

match :: Parser Term
match = do
  keyword "match"
  t0 <- expr
  keyword "with"

  void $ optional $ symbol "|"

  v1 <- var
  symbol "->"
  t1 <- expr

  symbol "|"

  v2 <- var
  symbol "->"
  t2 <- expr

  keyword "end"
  return $ Fix $ Term.Case t0 v1 t1 v2 t2

expr :: Parser Term
expr = choice
  [ (Fix .) . Term.Abs <$> (fun *> var <* symbol "->") <*> expr
  , (Fix .) . Term.Fix <$> (keyword "fix" *> var <* symbol "->") <*> expr
  , (Fix .) . Term.Gen <$> (gen *> tvar <* symbol "->") <*> expr
  , match
  , aexpr >>= \t -> do
      v <- optional $ symbol ":"
      case v of
        Just () -> Fix . Term.Ascribe t <$> typeParser
        Nothing -> return t
  ]

typeParser :: Parser Type
typeParser = choice
  [ typeBin
  , (Fix .) . Type.Forall <$> (symbol "∀" *> tvar <* symbol ".") <*> typeParser
  , (Fix .) . Type.Rec <$> (symbol "μ" *> tvar <* symbol ".") <*> typeParser
  , ((Fix . Type.XType) .) . Type.D <$> (symbol "Д" *> evar <* symbol ".") <*> typeParser
  ]

table :: [[Operator Parser Type]]
table =
  [ [binary "*" (\x y -> Fix $ Type.Prod x y)]
  , [binary "+" (\x y -> Fix $ Type.Sum x y)]
  , [binary "->" (\x y -> Fix $ Type.Arrow x y)]
  ]

binary :: T.Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL $ f <$ symbol name

typeBin :: Parser Type
typeBin = choice
  [ makeExprParser typeAtom table
  ]

typeAtom :: Parser Type
typeAtom = choice
  [ symbol "1" $> Fix Type.Unit
  , try $ Fix . Type.Var <$> tvar
  , parens typeParser
  , ((Fix . Type.XType) .) . Type.Suspend <$> (evalOrder <* symbol "|>") <*> typeAtom
  ]

evalOrder :: Parser (EvalOrder Surface)
evalOrder = choice
  [ EVar <$> evar
  , keyword "V" $> V
  , keyword "N" $> N
  ]
