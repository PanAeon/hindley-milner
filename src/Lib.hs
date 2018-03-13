module Lib
    ( someFunc
    ) where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

{- REFERENCES:
1. Very intuitive explanation
  http://www.codecommit.com/blog/scala/what-is-hindley-milner-and-why-is-it-cool
2. 1986 paper with some further references
  http://lucacardelli.name/Papers/BasicTypechecking.pdf
3. OCAml paper
  http://steshaw.org/hm/hindley-milner.pdf
4. short presentation
  https://www7.in.tum.de/um/courses/seminar/sove/SS2013/final/hindley-milner.slides.pdf

5. lame tutorial
  http://akgupta.ca/blog/2013/05/14/so-you-still-dont-understand-hindley-milner/

6. haskell example
   http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf

7. Another haskell example
  https://github.com/quchen/articles/tree/master/hindley-milner

8. Yet another haskell example
  http://dev.stephendiehl.com/fun/006_hindley_milner.html

-}

-- TODO: https://github.com/ollef/Earley

-- lambda calculus syntax

type Name = String

data Exp = Var Name
         | Lam Name Exp
         | App Exp Exp
         | Let Name Exp Exp -- let x = e1 in e2
         | Lit Lit
         deriving Show

         -- | If Expr Expr Expr
         -- | Fix Expr
         -- | Op BinOp Expr Expr

data Lit = LInt Integer
         | LBool Bool
         deriving (Show, Eq, Ord)

-- data BinOp = Add | Sub | Mul | Eql deriving (Show, Eq, Ord)

data Type = TVar String -- free variable
          | TInt
          | TBool
          | TFun Type Type
          deriving (Show, Eq)

--------------------------------------------------------------------------------

-- parser
type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal



rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["true", "false", "let","in"] -- "if","then","else",

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

lambdaParser :: Parser Exp
lambdaParser = exprParser' <* eof


exprParser' :: Parser Exp
exprParser' = appParser


appParser :: Parser Exp
appParser = do
             appsOrExpr <- (,) <$> exprParser <*> many exprParser
             return $ case appsOrExpr of
                         (e1, []) -> e1
                         (e1, xs) ->  foldl1 App $ e1:xs

--appParser = App <$> (exprParser) <*> exprParser



exprParser :: Parser Exp
exprParser =   varParser
             <|> lamParser
             <|> letParser
             <|> litParser
             <|> parens exprParser'

varParser :: Parser Exp
varParser = Var <$> identifier


-- \\x.x
lamParser :: Parser Exp
lamParser = do
            v <- (symbol "\\" <|> symbol "λ") *> identifier <* symbol "."
            e <- exprParser'
            return $ Lam v e

letParser :: Parser Exp
letParser = do
            rword "let" <* sc
            n <- identifier
            symbol "="
            e1 <- exprParser'
            rword "in"
            e2 <- exprParser'
            return $ Let n e1 e2

litParser :: Parser Exp
litParser = ((Lit . LBool) <$> ((True <$ symbol "true") <|> (False <$ symbol "false")))
            <|> ((Lit . LInt) <$> integer)

-- TODO: because of left factoring this is a bit ugly
-- understand left-factoring better, plz
-- FIXME: unit tests?
--------------------------------------------------------------------------------

typeExpression :: Exp -> Type
typeExpression = undefined

-- I don't know better way for now, FIXME: think of smth better

-- hm, maybe don't need the labels if go recursive
-- last constrain, one but last - assigned type
data LExp = LExp Exp [LExp] Type [(Type, Type)]

-- TODO: general recursive scheme?
data AExp = AVar Name Type
         | ALam Name Exp Type
         | AApp Exp Exp Type
         | ALet Name Exp Exp Type -- let x = e1 in e2
         | ALit Lit Type
         deriving Show

--
assignLabels :: Exp -> LExp
assignLabels e@(Lit (LInt _))    = LExp e [] TInt []
assignLabels e@(Lit (LBool _))   = LExp e [] TBool []
assignLabels e@(App e1 e2) =
  undefined
  where
    (LExp _ _ t1 c1) = assignLabels e1
    (LExp _ _ t2 c2) = assignLabels e2
assignLabels e@(Lam name e1) =
  undefined
  where
     (LExp _ _ t1 c1) = assignLabels e1
assignLabels e@(Let name e1 e2) =
  undefined
  where
    (LExp _ _ t1 c1) = assignLabels e1
    (LExp _ _ t2 c2) = assignLabels e2

-- Var Name
--          | Lam Name Exp
--          | App Exp Exp
--          | Let Name Exp Exp -- let x = e1 in e2
foo n f x = f (n f x)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
