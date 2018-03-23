module Lib
    ( someFunc,
      reconcile,
      doSomeWork
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
import Data.Maybe(maybeToList)
import Debug.Trace
import Data.List

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
         | Let Name Exp Exp -- let x = e1 in e2; !! let polymorphism!!
         | Letrec Name Exp Exp
         | Lit Lit
         deriving Show

         -- | If Expr Expr Expr
         -- | Fix Expr
         -- | Op BinOp Expr Expr

data Lit = LInt Integer
         | LBool Bool
         deriving (Show, Eq, Ord)

-- data BinOp = Add | Sub | Mul | Eql deriving (Show, Eq, Ord)

data Type = TVar Int -- free variable (lambda)
          | TInt
          | TBool
          | TFun Type Type
          deriving (Show, Eq, Ord)

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
rws = ["true", "false", "let","in", "letrec"] -- "if","then","else",

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
             <|> letrecParser
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


letrecParser :: Parser Exp
letrecParser = do
            rword "letrec" <* sc
            n <- identifier
            symbol "="
            e1 <- exprParser'
            rword "in"
            e2 <- exprParser'
            return $ Letrec n e1 e2

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
appL = stupdidParser "\\a.\\b.a b"
appL' = stupdidParser "\\a.\\b.b a"


-- I don't know better way for now, FIXME: think of smth better

-- hm, maybe don't need the labels if go recursive
-- last constrain, one but last - assigned type
data LExp = LExp Exp [LExp] Type [(Type, Type)]



type Constraint = (Type, Type)




freshTypeName :: State (Int, Map String Type) Int
freshTypeName = do
                 (i, m) <- get
                 put ((i+1), m)
                 pure i

assignLabels :: Exp -> State (Int, Map Name Type) (Type, [Constraint], [Type])
assignLabels e@(App e1 e2) =
  do
    (t1, c1, v1) <- assignLabels e1
    (t2, c2, v2) <- assignLabels e2
    tname <- freshTypeName -- FIXME: should I generate the new type or reuse type of (result) e1?
    let
        t' = TVar tname

        v' = v1 ++ v2
        c' = [ (t1, TFun t2 t')
             ] ++ c1 ++ c2 -- FIXME: faster merge?
    pure (t', c', v')

-- FIXME: variable shadowing !!!! ??? yes, or no?
-- FIXME: should be (lexically??) scoped.
-- during instantiation?

--"\\h. (let f = \\x.h x in  (f true) )"
assignLabels (Let name e1 e2) =
  do
    let e = replaceAllOccurrencesWithFreshNames name e1 e2
    (_, m) <- get
    (pabeda, cx, vx) <- assignLabels e
    tname <- freshTypeName
    let m' = M.delete name m
        t' = TVar tname

        cx' = (t', pabeda) : cx
    (i, _) <- get
    put (i, m') -- FIXME: ugly, shadowing (or maybe not for lambda but not let...)
    pure (t', cx', vx)

assignLabels (Letrec name e1 e2) =
  do -- ha! what is the type of name in e1?
     -- below won't work?
    let e = replaceAllOccurrencesWithFreshNames name e1 e2
    (_, m) <- get
    (pabeda, cx, vx) <- assignLabels e
    tname <- freshTypeName
    let m' = M.delete name m

        t' = TVar tname

        cx' = (t', pabeda) : cx
    (i, _) <- get
    put (i, m') -- FIXME: ugly, shadowing (or maybe not for lambda but not let...)
    pure (t', cx', vx)

assignLabels e@(Lam name e1) =
  do
    tx <- TVar <$> freshTypeName
    (i', m') <- get
    put (i', M.insert name tx m')
    (t1, c1, v1) <- assignLabels e1
    (_, m) <- get
    tname <- freshTypeName -- fresh type name is necessary
    -- tx    <- if M.member name m
    --          then pure $  m ! name
    --          else TVar <$> freshTypeName
    let m' = M.delete name m

        t' = TVar tname


        c' = [ (t', TFun tx t1) -- FIXME: is this right _squint_ , seems to be so
             ] ++ c1
    (i, _) <- get
    put (i, m') -- FIXME: fucking shame
    pure (t',  c', tx:v1)
assignLabels (Var name) =
  do
  (_, m) <- get
  if (M.member name m)
  then let
       t' = maybe undefined id (M.lookup name m)
       in pure (t', [], [])
  else do
        tname <- freshTypeName
        (i,m) <- get
        let
          t' = TVar tname
        put (i, M.insert name t' m)
        pure (t', [], [])

assignLabels (Lit (LInt i)) = pure ( TInt, [], [])
assignLabels (Lit (LBool i)) = pure (TBool, [], [])

-- again, what to do with shadowing? (TBD)


replaceAllOccurrencesWithFreshNames :: String -> Exp -> Exp -> Exp
replaceAllOccurrencesWithFreshNames name e1 (Lam n' b) = if n' == name
                                                         then (Lam n' b)
                                                         else (Lam n' (replaceAllOccurrencesWithFreshNames name e1 b))
replaceAllOccurrencesWithFreshNames name e1 (App a b) = App (replaceAllOccurrencesWithFreshNames name e1 a) (replaceAllOccurrencesWithFreshNames name e1 b)
replaceAllOccurrencesWithFreshNames name e1 (Let n' a b) = Let n' (replaceAllOccurrencesWithFreshNames name e1 a) (if n' == name
                                                           then b
                                                           else (replaceAllOccurrencesWithFreshNames name e1 b))
replaceAllOccurrencesWithFreshNames name e1 (Letrec n' a b) = if n' == name
                                                              then (Letrec n' a b)
                                                              else (Letrec n' (replaceAllOccurrencesWithFreshNames name e1 a) (replaceAllOccurrencesWithFreshNames name e1 b))
replaceAllOccurrencesWithFreshNames name e1 (Lit x) = Lit x
replaceAllOccurrencesWithFreshNames name e1 (Var x) = if x == name
                                                      then e1
                                                      else (Var x)





getTypeVarName :: Type -> Int
getTypeVarName (TVar i) = i
getTypeVarName _ = error "getTypeVarName works only with TVar !"
-- FUCK: this shit works!!!
-- FIXME: circular dependencies? -- check !! if so yield "can not construct infinite type"

-- very basic just explode all functional types:


solveConstraints :: [Constraint] -> Type
solveConstraints xs = let
                       (a, b , xs') = solveSingleConstraint xs
                       in if null xs'
                          then  b
                          else solveConstraints xs'









traceConstraints :: String -> [Constraint] -> a -> a
traceConstraints marker cs rest = trace y rest
   where
     cs' = sortOn (getTypeVarName . fst)  cs
     cs'' = f <$> cs'
     f (TVar i, t) = "t" ++ show i ++ " :: " ++ pprint t
     y = marker ++ ": <" ++ ((intersperse "; " cs'') >>= id) ++ ">"



solveSingleConstraint :: [Constraint] -> (Type, Type, [Constraint])
solveSingleConstraint xs =
                           let
                             zs = traceConstraints "solveSingleConstraint" xs $ groupBy (\a b -> getTypeVarName (fst a) == getTypeVarName (fst b)) $ sortOn (getTypeVarName . fst)   xs
                             y  = head zs
                             ys =  tail zs >>= id
                             (ty, tr, cs') = unifySingle y
                             ys' = replaceType ys ty tr
                           in (ty, tr, cs' ++ ys')







    -- now replace ty with tr, + addd constraints , resort, merge, repeat



replaceType :: [Constraint] -> Type -> Type -> [Constraint]
replaceType cs t t2 = (\x -> (fst x,  f $ snd x)) <$> cs
  where
    f (TFun a b) = TFun (f a) (f b)
    f x@(TVar _) = if getTypeVarName x == getTypeVarName t
                     then t2
                     else x
    f x          = x

isTFun :: Type -> Bool
isTFun (TFun _ _) = True
isTFun _          = False

isTPrim :: Type -> Bool
isTPrim TInt = True
isTPrim TBool = True
isTPrim _     = False


-- foobar = unifySingle [
--            (TVar 1, TFun (TVar 2) (TVar 3))
--          , (TVar 1, TFun (TVar 4) (TFun (TVar 6) (TVar 7)))
--          ]


unifySingle :: [Constraint] ->  (Type, Type, [Constraint])
-- unifySingle (x:[]) = (fst x, snd x, [x]) -- FIXME: bug ... side-effecty
unifySingle (x:xs) =
                     let
                       (resType, constraints) = foldl f (snd x, []) (snd <$> xs)
                       f (t, cs) t1 = let
                                        (t', cs') =  reconcile t t1  -- and solve against free vars ...

                                       in (t', cs ++ cs')

                     in (fst x, resType, constraints)


getFreeVars :: Type -> [Type]
getFreeVars (TFun a b) = getFreeVars a ++ getFreeVars b
getFreeVars (TVar i) = [TVar i]
getFreeVars _        = []







-- TODO: unit tests?
reconcile:: Type -> Type ->  (Type, [Constraint])
reconcile (TFun a b) (TFun c d) = let
                                    (a1, c1) = reconcile a c
                                    (b1, c2) = reconcile b d
                                  in (TFun a1 b1, c1 ++ c2)

reconcile (TVar i) (TVar j) | i == j =  (TVar i, [])
                            | otherwise =  (TVar i, [(TVar j, TVar i)])
reconcile (TVar i) t2 =  (TVar i ,[(TVar i, t2)])
reconcile t2 (TVar i) =  (TVar i ,[(TVar i, t2)])
reconcile TInt TInt =  (TInt, [])
reconcile TBool TBool =  (TBool, [])
reconcile a b = error $ "Could not match :" ++ show a ++ " with " ++ show b



-- "let app = \\f.\\x.f x in \\g.\\y. app g y"
-- "letrec = \\x.v x in \\y.v y"
-- "\\f.\\g.g (f true) (f 0)" works
-- "let f = \x.x in \\g.g (f true) (f false)"
-- \\g.\\x.\\f.g (f false) (f x)
-- FIXME: let doesn't work in:
-- "let f = \\x.x in \\g.g (f true) (f 0)"

-- "letrec v = \\a.(v a) in v"
-- \\f.\\a.(letrec v = v (f a) in v)

--  \\f.\\a.(letrec v = \\.x f (v x) in v a) -- wow! right type, wrong expression
-- "let f = \\x.x in \\g.g (f true) (f 0)" -- right, "(Bool -> Int -> t) -> t"

-- FIXME: wrong, should fail!!!
-- "\\h. (let f = \\x.h x in \\g.g (f true) (f 1))" -- wrong!
doSomeWork = pprint $ normalizeTypeNames res
  where -------- fuck, it just replaced h with g
        -- can we check that output doesn't contains any g's?
        -- add check that concrete != abstract?
   e0 = stupdidParser "let app = \\f.\\x.f x in \\g.\\y. app g y"--"let f = \\x.x in \\g.g (f true) (f 0)"
   ((expr, xs, _), _) = runState (assignLabels e0) (0, M.empty)
   res = solveConstraints xs
   -- (ALam p b t) = expr
   -- t' = maybe undefined id (M.lookup t constraints)
   -- t'' = normalizeTypeNames t'



-- right associativeness!!
--- "((t1 -> t2) -> (t1 -> t2))" !!!


pprint :: Type -> String
pprint (TVar name) = "t" ++ if name == 0 then "" else show name
pprint (TFun tf@(TFun a' b') b) = "(" ++ pprint tf ++ ")" ++ " -> " ++ pprint b
pprint (TFun a b) =  pprint a ++ " -> " ++ pprint b
pprint (TBool) = "Bool"
pprint (TInt)  = "Int"

lkp k m = maybe (error "no such key") id (M.lookup k m)

normalizeTypeNames:: Type -> Type
normalizeTypeNames t = f t
  where
    xs = sort $ getTypeVarNames t
    xs' = zip xs [0..]
    m   = M.fromList xs'
    f (TVar i) = TVar $ (lkp i m)
    f (TFun a b) = TFun (f a) (f b)
    f x        = x


getTypeVarNames :: Type -> [Int]
getTypeVarNames = nub . getTypeVarNames'

-- FIXME: fold?
getTypeVarNames' :: Type -> [Int]
getTypeVarNames' (TVar i) = [i]
getTypeVarNames' TInt = []
getTypeVarNames' TBool = []
getTypeVarNames' (TFun a b) = getTypeVarNames a ++ getTypeVarNames b


foo n f x = f (n f x)
foo' n f x = n f x


baz a b = a b

buz = let
       f x = x
     in \g -> g (f 2) (f True)

-- buz2 h = let
--           f x = h x
--          in \g -> g (f 1) (f True)
-- let v = \\a.(v a) in v
-- \\f.\\a.(let v = v (f a) in v)
buzz f a = f (f a)

-- \\f.\\a.(letrec v = \\.x f (v x) in v a)

buzz' f a = let
             v = \x -> v (f x)
            in v a



--fuz f = f f -- cannot construct infinite type

someFunc :: IO ()
someFunc = putStrLn "someFunc"
