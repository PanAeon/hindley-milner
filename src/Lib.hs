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
          | TGen Int -- super free variable (let)
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

-- TODO: general recursive scheme?
-- FIXME: get rid of AExp ???
data AExp = AVar Name Type
         | ALam Name AExp Type
         | AApp AExp AExp Type
         | ALet Name AExp AExp Type -- let x = e1 in e2
         | ALetrec Name AExp AExp Type
         | ALit Lit Type
         deriving Show

type Constraint = (Type, Type)

getType :: AExp -> Type
getType (AVar _ t) = t
getType (ALam _ _ t) = t
getType (AApp _ _ t) = t
getType (ALet _ _ _ t) = t
getType (ALit _ t) = t
getType (ALetrec _ _ _ t) = t



freshTypeName :: State (Int, Map String Type) Int
freshTypeName = do
                 (i, m) <- get
                 put ((i+1), m)
                 pure i

assignLabels :: Exp -> State (Int, Map Name Type) (AExp, [Constraint])
assignLabels e@(App e1 e2) =
  do
    (e1', c1) <- assignLabels e1
    (e2', c2) <- assignLabels e2
    tname <- freshTypeName -- FIXME: should I generate the new type or reuse type of (result) e1?
    let
        t1 = getType e1'
        t2 = getType e2'
        t' = TVar tname
        e' = AApp e1' e2' t'
        c' = [ (t1, TFun t2 t')
             ] ++ c1 ++ c2 -- FIXME: faster merge?
    pure (e', c')

-- FIXME: variable shadowing !!!! ??? yes, or no?
assignLabels (Let name e1 e2) =
  do
    (e1', c1) <- assignLabels e1
    (e2', c2) <- assignLabels e2
    (_, m) <- get
    tx    <- if M.member name m
             then pure $  TVar $ getTypeVarName $ m ! name
             else TVar <$> freshTypeName
    tname <- freshTypeName
    let m' = M.delete name m
        t1 =  getType e1'
        t2 = getType e2'
        -- tx' = TGen $ getTypeVarName $ t1
        c1' = rewriteAsGen t1 c1
        t' = TVar tname
        e' = ALet name e1' e2' t'
        c' = [(t1, tx), (t', t2)] ++ c1' ++ c2 -- maybe the problem is with solver (e1 should be resolved before e2)
    (i, _) <- get
    put (i, m') -- FIXME: ugly, shadowing (or maybe not for lambda but not let...)
    pure (e', c')

--- letrec v = e1 in e2 === let v = fix(\v.e1) in e2
-- assignLabels (Letrec name e1 e2) = -- FIXME: same as let?
--   do
--     (e1', c1) <- assignLabels e1 ---- aha, should check for e1
--     (e2', c2) <- assignLabels e2
--     (_, m) <- get
--     tname <- freshTypeName
--     tx <- if M.member name m -- if name in e2
--           then pure $ m ! name -- get it's name
--           else TVar <$> freshTypeName -- or assign new name
--     let m' = M.delete name m
--         t1 = getType e1'
--         t2 = getType e2'
--         t' = TVar tname
--         e' = ALetrec name e1' e2' t'
--         c' = [(t', t2),(tx, t1)] ++ c1 ++ c2
--     (i, _) <- get
--     put (i, m') -- FIXME: ugly, shadowing (or maybe not for lambda but not let...)
--     pure (e', c')

assignLabels e@(Lam name e1) =
  do
    (e1', c1) <- assignLabels e1
    (_, m) <- get
    tname <- freshTypeName -- fresh type name is necessary
    tx    <- if M.member name m
             then pure $  m ! name
             else TVar <$> freshTypeName
    let m' = M.delete name m
        t1 = getType e1'
        t' = TVar tname
        e' = ALam name e1' t'

        c' = [ (t', TFun tx t1) -- FIXME: is this right _squint_ , seems to be so
             ] ++ c1
    (i, _) <- get
    put (i, m') -- FIXME: fucking shame
    pure (e',  c')
assignLabels (Var name) =
  do
  (_, m) <- get
  if (M.member name m)
  then let
       t' = maybe undefined id (M.lookup name m)
       e' = AVar name t'
       in pure (e', [])
  else do
        tname <- freshTypeName
        (i,m) <- get
        let
          t' = TVar tname
          e' = AVar name t'
        put (i, M.insert name t' m)
        pure (e', [])

assignLabels (Lit (LInt i)) = pure (ALit (LInt i) TInt, [])
assignLabels (Lit (LBool i)) = pure (ALit (LBool i) TBool, [])


rewriteAsGen :: Type -> [Constraint] -> [Constraint]
rewriteAsGen t@(TVar i) = fmap f
   where
     f (t1, t2) | t1 == t = (TGen i, t2)
     f c                  = c
rewriteAsGen _ = error "Could not rewrite other value but TVar"


getTypeVarName :: Type -> Int
getTypeVarName (TVar i) = i
getTypeVarName (TGen i) = i
getTypeVarName _ = error "getTypeVarName works only with TVar/TGen !"
-- FUCK: this shit works!!!
-- FIXME: circular dependencies? -- check !! if so yield "can not construct infinite type"
-- solve constraints (how?)
-- very basic just explode all functional types:


solveConstraints :: [Constraint] -> Type
solveConstraints xs = let
                        (a, b , xs') = solveSingleConstraint xs
                      in if null xs'
                         then b
                         else solveConstraints xs'


solveGenConstraints:: [Int] -> [Constraint] -> [Constraint]
solveGenConstraints ns xs = foo (length fs) cs
   where
     (fs, rs) = partition (\x ->   elem (getTypeVarName $ fst x) ns) xs -- (isTGen (fst x)) &&
     cs = fs ++ rs
     foo 0 xs = xs
     foo n xs = let
                  (_, _, xs') = solveSingleConstraint xs
                in foo (n-1) xs'



isTGen :: Type -> Bool
isTGen (TGen _) = True
isTGen _ = False

isCTGen :: [Constraint] -> Bool
isCTGen = any (isTGen . fst)


traceConstraints :: String -> [Constraint] -> a -> a
traceConstraints marker cs rest = trace y rest
   where
     cs' = sortOn (getTypeVarName . fst)  cs
     cs'' = f <$> cs'
     f (TVar i, t) = "t" ++ show i ++ " :: " ++ pprint t
     f (TGen i, t) = "g" ++ show i ++ " :: " ++ pprint t
     y = marker ++ ": <" ++ ((intersperse "; " cs'') >>= id) ++ ">"

headTrace :: String -> [a] -> a
headTrace marker xs = if null xs
               then error ("Empty list at: " ++ marker)
               else head xs

solveSingleConstraint :: [Constraint] -> (Type, Type, [Constraint])
solveSingleConstraint xs =   (ty, tr, cs' ++ ys')
  where
    zs = traceConstraints "solveSingleConstraint" xs $ groupBy (\a b -> getTypeVarName (fst a) == getTypeVarName (fst b)) $ sortOn (getTypeVarName . fst)   xs
    y  = let
           (gs,rs) = partition (isTGen . fst) (headTrace "solveSingle" zs)
         in gs ++ rs
    ys =  tail zs >>= id


    (ty, tr, cs') = if isCTGen y
                    then unifyGen (fst $ headTrace "y" y) (snd $ headTrace "y" y) (snd <$> (tail y))
                    else unifySingle y
    -- tr' = if isCTGen y
    --       then replaceVarsWithTGen tr
    --       else tr
    ys' = replaceType ys ty tr
    -- now replace ty with tr, + addd constraints , resort, merge, repeat

-- FIXME: replace unbound vars?
replaceVarsWithTGen :: Type -> Type
replaceVarsWithTGen (TVar x) = TGen x
replaceVarsWithTGen (TFun a b) = TFun (replaceVarsWithTGen a) (replaceVarsWithTGen b)
replaceVarsWithTGen x = x

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


unifySingle :: [Constraint] -> (Type, Type, [Constraint])
unifySingle (x:xs) =
                       (fst x, resType, constraints)

  where
    -- tx = (trace $ "reconcile: " ++ show xs) $ fst $ head xs
    (resType, constraints) = foldl f (snd x, []) (snd <$> xs)
    f (t0, cs) t1 = let
                     (t, cs1) = reconcile t0 t1
                    in (t, cs ++ cs1)

unifyGen :: Type -> Type -> [Type] -> (Type, Type, [Constraint])
unifyGen tid tbody cs = (tid, tbody, constraints)
  where
    constraints =  foldl f ([]) (cs)
    freeVars = getFreeVars tbody
    freeNames = getTypeVarName <$> freeVars
    f cs t1 = let
                     (_, cs') = reconcile tbody t1
                     -- solve against free vars in tgen, then all leftover
                     cs'' = solveGenConstraints freeNames cs'

                     cs''' = {-filter (\x -> notElem (getTypeVarName (fst x)) freeNames)-} (foo <$> cs'')
                     foo (TVar x, y) = (TGen x, y)

                    -- cs''' = (\x -> (fst x, generifyFreeVars freeNames (snd x))) <$> cs''
                    in  ( cs''' ++ cs)


getFreeVars :: Type -> [Type]
getFreeVars (TFun a b) = getFreeVars a ++ getFreeVars b
getFreeVars (TVar i) = [TVar i]
getFreeVars _        = []

generifyFreeVars :: [Int] -> Type -> Type
generifyFreeVars fs (TVar x) | elem x fs = TGen x
                             | otherwise = TVar  x
generifyFreeVars fs (TFun a b) = TFun (generifyFreeVars fs a) (generifyFreeVars fs b)
generifyFreeVars fs x = x

-- we know that to the left is TGEN, to the right is not
-- reconcile'' :: Type -> Type -> [Constraint]
-- reconcile'' (TFun a b) (TFun c d) = let
--                                       c1 = reconcile'' a c
--                                       c2 = reconcile'' b d
--                                     in (c1 ++ c2)
-- reconcile'' (TVar i) (TVar j) | i == j = []
--                             | otherwise = [(TVar i, TVar j)]
-- reconcile'' (TVar i) t2 = [(TVar i, t2)]
-- reconcile'' t2 (TVar i) = [(TVar i, t2)] -- TODO: add gen to t2 here ?!
-- reconcile'' TInt TInt = []
-- reconcile'' TBool TBool = []
-- reconcile'' a b = error $ "(reconcile'') Could not match :" ++ show a ++ " with " ++ show b


-- reconcile' (TFun a b) (TFun c d) = let
--                                     (a1, c1) = reconcile a c
--                                     (b1, c2) = reconcile b d
--                                   in (TFun a1 b1, c1 ++ c2)
-- -- reconcile (TGen a) b = genericReconcile (TGen a) b
--
-- reconcile' (TVar i) (TVar j) | i == j = (TVar i, [])
--                             | otherwise = (TVar i, [(TVar j, TVar i)])
-- reconcile' (TVar i) t2 = (TVar i ,[(TVar i, t2)])
-- reconcile' t2 (TVar i) = (TVar i ,[(TVar i, t2)])
-- reconcile' TInt TInt = (TInt, [])
-- reconcile' TBool TBool = (TBool, [])
-- reconcile' a b = error $ "Could not match :" ++ show a ++ " with " ++ show b

-- TODO: unit tests?
reconcile:: Type -> Type -> (Type, [Constraint])
reconcile (TFun a b) (TFun c d) = let
                                    (a1, c1) = reconcile a c
                                    (b1, c2) = reconcile b d
                                  in (TFun a1 b1, c1 ++ c2)
-- reconcile (TGen a) b = genericReconcile (TGen a) b
reconcile (TVar i) (TVar j) | i == j = (TVar i, [])
                            | otherwise = (TVar i, [(TVar j, TVar i)])
reconcile (TVar i) t2 = (TVar i ,[(TVar i, t2)])
reconcile t2 (TVar i) = (TVar i ,[(TVar i, t2)])
reconcile TInt TInt = (TInt, [])
reconcile TBool TBool = (TBool, [])
reconcile a b = error $ "Could not match :" ++ show a ++ " with " ++ show b

-- genericReconcile :: Type -> Type -> (Type, [Constraint])
-- genericReconcile t0 t1 = (t0, cs)
--     where





-- "let app = \\f.\\x.f x in \\g.\\y. app g y"
-- "letrec = \\x.v x in \\y.v y"
-- "\\f.\\g.g (f true) (f 0)" works
-- "let f = \x.x in \\g.g (f true) (f false)"
-- \\g.\\x.\\f.g (f false) (f x)
-- FIXME: let doesn't work in:
-- "let f = \\x.x in \\g.g (f true) (f 0)"

-- "letrec v = \\a.(v a) in v"
-- \\f.\\a.(letrec v = v (f a) in v) -- wrong!!!!!!!!!!!!!!!!!!!

--  \\f.\\a.(letrec v = \\.x f (v x) in v a) -- wow! right type, wrong expression
-- "let f = \\x.x in \\g.g (f true) (f 0)" -- right, "(Bool -> Int -> t) -> t"
doSomeWork = pprint $ normalizeTypeNames res
  where
   e0 = stupdidParser "\\h. (let f = \\x.h x in \\g.g (f true) (f 1))"
   ((expr, xs), _) = runState (assignLabels e0) (0, M.empty)
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
