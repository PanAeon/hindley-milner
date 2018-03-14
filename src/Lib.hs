module Lib
    ( someFunc
    ) where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State
import Data.Map(Map, (!))
import qualified Data.Map.Lazy as M

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

-- parseTest lambdaParser "λn.λf.λx.f (n f x)"
--------------------------------------------------------------------------------
addS =  "\\n.\\f.\\x.f (n f x)"
-- runState (assignLabels addL) 0
stupdidParser x = maybe (error "can not parse") id  $ parseMaybe  lambdaParser x
addL =  stupdidParser addS
appL = stupdidParser "a b"


typeExpression :: Exp -> Type
typeExpression = undefined

-- I don't know better way for now, FIXME: think of smth better

-- hm, maybe don't need the labels if go recursive
-- last constrain, one but last - assigned type
data LExp = LExp Exp [LExp] Type [(Type, Type)]

-- TODO: general recursive scheme?
data AExp = AVar Name Type
         | ALam Name AExp Type
         | AApp AExp AExp Type
         | ALet Name AExp AExp Type -- let x = e1 in e2
         | ALit Lit Type
         deriving Show

type Constraint = (Type, Type)

getType :: AExp -> Type
getType (AVar _ t) = t
getType (ALam _ _ t) = t
getType (AApp _ _ t) = t
getType (ALet _ _ _ t) = t
getType (ALit _ t) = t

freshTypeName :: State Int String
freshTypeName = do
                 i <- get
                 put (i+1)
                 pure $ "t" ++ if i == 0 then "" else show i

assignLabels :: Exp -> State Int (AExp, Map Name Type, [Constraint])
assignLabels e@(App e1 e2) =
  do
    (e1', m1, c1) <- assignLabels e1
    (e2', m2, c2) <- assignLabels e2
    tname <- freshTypeName -- FIXME: should I generate the new type or reuse type of (result) e1?
    let m' = M.union m1 m2
        t1 = getType e1'
        t2 = getType e2'
        t' = TVar tname
        e' = AApp e1' e2' t'
        c' = [ (t1, TFun t2 t')
             ] ++ c1 ++ c2 -- FIXME: faster merge?
    pure (e', m', c')




assignLabels e@(Lam name e1) =
  do
    (e1', m1, c1) <- assignLabels e1
    tname <- freshTypeName -- FIXME: and here should I reuse the result of e1?
    tx    <- if M.member name m1
             then pure $  m1 ! name
             else TVar <$> freshTypeName
    let m' = M.delete name m1
        t1 = getType e1'
        t' = TVar tname
        e' = ALam name e1' t'

        c' = [ (t', TFun tx t1) -- FIXME: is this right _squint_ , seems to be so
             ] ++ c1
    {-
      constraints:
      tname => X -> Y
      if M contains name then .. ....
    -}
    pure (e', m', c')
assignLabels e@(Var name) =
  (\tname ->
    let
    t' = TVar tname
    e' = AVar name t'
    in (e', M.singleton name t', [])
  ) <$> freshTypeName




--
-- assignLabels :: Exp -> LExp
-- assignLabels e@(Lit (LInt _))    = LExp e [] TInt []
-- assignLabels e@(Lit (LBool _))   = LExp e [] TBool []
-- assignLabels e@(App e1 e2) =
--   undefined
--   where
--     (LExp _ _ t1 c1) = assignLabels e1
--     (LExp _ _ t2 c2) = assignLabels e2
-- assignLabels e@(Lam name e1) =
--   undefined
--   where
--      (LExp _ _ t1 c1) = assignLabels e1
-- assignLabels e@(Let name e1 e2) =
--   undefined
--   where
--     (LExp _ _ t1 c1) = assignLabels e1
--     (LExp _ _ t2 c2) = assignLabels e2

-- Var Name
--          | Lam Name Exp
--          | App Exp Exp
--          | Let Name Exp Exp -- let x = e1 in e2
foo n f x = f (n f x)
bar n = z
  where
    z =  \f x -> f (n f x)

baz a b = a b
someFunc :: IO ()
someFunc = putStrLn "someFunc"
