{-# LANGUAGE PatternGuards, TupleSections, ViewPatterns #-}
module Core.Parser (parse) where

import Core.Syntax
import Core.Prelude

import Name hiding (freshName)
import qualified Name
import StaticFlags
import Utilities

import qualified Data.Map as M

import qualified Language.Haskell.Exts as LHE
import Language.Preprocessor.Cpphs

import System.Directory
import System.FilePath (replaceExtension)


parse :: FilePath -> IO (String, [(Var, Term)])
parse path = do
    -- Read and pre-process .core file
    contents <- readFile path >>= cpp
    unless qUIET $ putStrLn contents
    
    -- Read and pre-process corresponding .hs file (if any)
    let wrapper_path = replaceExtension path ".hs"
    has_wrapper <- doesFileExist wrapper_path
    wrapper <- if has_wrapper then readFile wrapper_path >>= cpp else return ""
    
    -- Return parsed .core file
    return (wrapper, moduleCore . LHE.fromParseResult . LHE.parseFileContentsWithMode (LHE.defaultParseMode { LHE.parseFilename = path, LHE.extensions = [LHE.EnableExtension LHE.CPP, LHE.EnableExtension LHE.MagicHash] }) $ contents)
  where cpp = runCpphs (defaultCpphsOptions { boolopts = (boolopts defaultCpphsOptions) { locations = False }, defines = ("SUPERCOMPILE", "1") : defines defaultCpphsOptions }) path


data ParseState = ParseState {
    ids :: IdSupply,
    dc_wrappers :: M.Map DataCon Var,
    int_wrappers :: M.Map Integer Var,
    char_wrappers :: M.Map Char Var,
    prim_wrappers :: M.Map PrimOp Var
  }

initParseState :: ParseState
initParseState = ParseState {
    ids = parseIdSupply,
    dc_wrappers = M.empty,
    int_wrappers = M.empty,
    char_wrappers = M.empty,
    prim_wrappers = M.empty
  }

buildWrappers :: ParseState -> [(Var, Term)]
buildWrappers ps
  = [ (f, lambdas xs $ data_ dc xs)
    | (dc, f) <- M.toList (dc_wrappers ps)
    , let arity = dataConArity dc; xs = map (\i -> name $ "x" ++ show i) [1..arity] ] ++
    [ (f, int i)
    | (i, f) <- M.toList (int_wrappers ps) ] ++
    [ (f, char c)
    | (c, f) <- M.toList (char_wrappers ps) ] ++
    [ (f, lam (name "x1") $ lam (name "x2") $ primOp pop [var (name "x1"), var (name "x2")])
    | (pop, f) <- M.toList (prim_wrappers ps) ] ++
    [ (name "error", lam (name "msg") $ case_ (var (name "prelude_error") `app` name "msg") []) ]
  where
    dataConArity "()"      = 0
    dataConArity "(,)"     = 2
    dataConArity "(,,)"    = 3
    dataConArity "(,,,)"   = 4
    dataConArity "[]"      = 0
    dataConArity "(:)"     = 2
    dataConArity "Left"    = 1
    dataConArity "Right"   = 1
    dataConArity "True"    = 0
    dataConArity "False"   = 0
    dataConArity "Just"    = 1
    dataConArity "Nothing" = 0
    dataConArity "MkU"     = 1 -- GHCBug
    dataConArity "Z"       = 0 -- Exp3_8
    dataConArity "S"       = 1 -- Exp3_8
    dataConArity "Leaf"    = 1 -- SumTree
    dataConArity "Branch"  = 2 -- SumTree
    dataConArity "Empty"   = 0 -- ZipTreeMaps
    dataConArity "Node"    = 3 -- ZipTreeMaps
    dataConArity "Wheel1"  = 2 -- Wheel-Sieve1
    dataConArity "Wheel2"  = 3 -- Wheel-Sieve2
    dataConArity "A"       = 0 -- KMP
    dataConArity "B"       = 0 -- KMP
    dataConArity s = panic "dataConArity" (text s)

newtype ParseM a = ParseM { unParseM :: ParseState -> (ParseState, a) }

instance Functor ParseM where
    fmap = liftM

instance Monad ParseM where
    return x = ParseM $ \s -> (s, x)
    mx >>= fxmy = ParseM $ \s -> case unParseM mx s of (s, x) -> unParseM (fxmy x) s

instance Applicative ParseM where
    pure x = ParseM $ \s -> (s, x)
    (<*>) = ap

freshName :: String -> ParseM Name
freshName n = ParseM $ \s -> let (ids', x) = Name.freshName (ids s) n in (s { ids = ids' }, x)

freshFloatName :: String -> Term -> ParseM (Maybe (Var, Term), Name)
freshFloatName _ (Term (Var x)) = return (Nothing, x)
freshFloatName n e              = freshName n >>= \x -> return (Just (x, e), x)

nameIt :: Term -> (Var -> ParseM Term) -> ParseM Term
nameIt e f = freshFloatName "a" e >>= \(mb_float, x) -> fmap (bind (maybeToList mb_float)) $ f x

nameThem :: [Term] -> ([Var] -> ParseM Term) -> ParseM Term
nameThem es f = mapM (freshFloatName "a") es >>= \(unzip -> (mb_es, xs)) -> fmap (bind (catMaybes mb_es)) $ f xs

list :: [Term] -> ParseM Term
list es = nameThem es $ \es_xs -> replicateM (length es) (freshName "list") >>= \cons_xs -> return $ uncurry bind $ foldr (\(cons_x, e_x) (floats, tl) -> ((cons_x, tl) : floats, cons e_x cons_x)) ([], nil) (cons_xs `zip` es_xs)

dataConWrapper :: DataCon -> ParseM Var
dataConWrapper = grabWrapper dc_wrappers (\s x -> s { dc_wrappers = x })

intWrapper :: Integer -> ParseM Var
intWrapper = grabWrapper int_wrappers (\s x -> s { int_wrappers = x })

charWrapper :: Char -> ParseM Var
charWrapper = grabWrapper char_wrappers (\s x -> s { char_wrappers = x })

primWrapper :: PrimOp -> ParseM Var
primWrapper = grabWrapper prim_wrappers (\s x -> s { prim_wrappers = x })

grabWrapper :: Ord a
            => (ParseState -> M.Map a Var) -> (ParseState -> M.Map a Var -> ParseState)
            -> a -> ParseM Var
grabWrapper get set what = do
    mb_x <- ParseM $ \s -> (s, M.lookup what (get s))
    case mb_x of Just x -> return x
                 Nothing -> freshName "wrap" >>= \x -> ParseM $ \s -> (set s (M.insert what x (get s)), x)

runParseM :: ParseM a -> ([(Var, Term)], a)
runParseM = first buildWrappers . flip unParseM initParseState


moduleCore :: (Show l) => LHE.Module l -> [(Var, Term)]
moduleCore (LHE.Module _loc _ops _mbexports _imports decls) = wrap_xes ++ xes
  where (wrap_xes, xes) = runParseM $ declsCore decls


declsCore :: (Show l) => [LHE.Decl l] -> ParseM [(Name, Term)]
declsCore = fmap concat . mapM declCore

declCore :: (Show l) => LHE.Decl l -> ParseM [(Name, Term)]
declCore (LHE.FunBind _loc [LHE.Match _loc' n pats (LHE.UnGuardedRhs _l e) (Just _binds@(LHE.BDecls _l' where_decls))]) = do
    let x = name (nameString n)
        (ys, _bound_ns, build) = patCores pats
    xes <- declsCore where_decls
    e <- expCore e
    return [(x, lambdas ys $ build $ bind xes e)]
declCore (LHE.FunBind _loc [LHE.Match _loc' n pats (LHE.UnGuardedRhs _l e) Nothing]) = 
    declCore (LHE.FunBind _loc [LHE.Match _loc' n pats (LHE.UnGuardedRhs _l e) (Just (LHE.BDecls _l []))])
declCore (LHE.PatBind _loc pat (LHE.UnGuardedRhs _l e) (Just _binds@(LHE.BDecls _l' where_decls))) = do
    let (x, bound_ns, build) = patCore pat
    xes <- declsCore where_decls
    e <- expCore e
    return $ (x, bind xes e) : [(n, build (var n)) | n <- bound_ns, n /= x]
declCore (LHE.PatBind _loc pat (LHE.UnGuardedRhs _l e) Nothing) =
    declCore (LHE.PatBind _loc pat (LHE.UnGuardedRhs _l e) (Just (LHE.BDecls _l [])))
declCore d = panic "declCore" (text $ show d)

expCore :: (Show l) => LHE.Exp l -> ParseM Term
expCore (LHE.Var _l qname) = qNameCore qname
expCore (LHE.Con _l qname) = fmap var $ dataConWrapper $ qNameDataCon qname
expCore (LHE.Lit _l lit) = literalCore lit
expCore (LHE.NegApp _l e) = expCore $ LHE.App _l (LHE.Var _l (LHE.UnQual _l (LHE.Ident _l "negate"))) e
expCore (LHE.App _l e1 e2) = expCore e2 >>= \e2 -> e2 `nameIt` \x2 -> fmap (`app` x2) $ expCore e1
expCore (LHE.InfixApp _l e1 eop e2) = expCore e1 >>= \e1 -> e1 `nameIt` \x1 -> expCore e2 >>= \e2 -> e2 `nameIt` \x2 -> qopCore eop >>= \eop -> return $ apps eop [x1, x2]
expCore (LHE.Let _l' (LHE.BDecls _l binds) e) = do
    xes <- declsCore binds
    fmap (bind xes) $ expCore e
expCore (LHE.If _l e1 e2 e3) = expCore e1 >>= \e1 -> liftM2 (if_ e1) (expCore e2) (expCore e3)
expCore (LHE.Case _l e alts) = expCore e >>= \e -> fmap (case_ e) (mapM altCore alts)
expCore (LHE.Tuple _l _b es) = mapM expCore es >>= flip nameThem (return . tuple)
expCore (LHE.Paren _l e) = expCore e
expCore (LHE.List _l es) = mapM expCore es >>= list
expCore (LHE.Lambda _ ps e) = expCore e >>= \e -> return $ lambdas xs $ build e
  where (xs, _bound_xs, build) = patCores ps
expCore (LHE.LeftSection _l e1 eop) = expCore e1 >>= \e1 -> e1 `nameIt` \x1 -> qopCore eop >>= \eop -> return (eop `app` x1) -- NB: careful about sharing if you add Right sections!
expCore (LHE.EnumFromThen _l e1 e2) = expCore $ LHE.App _l (LHE.App _l (LHE.Var _l (LHE.UnQual _l (LHE.Ident _l "enumFromThen"))) e1) e2
expCore e = panic "expCore" (text $ show e)

qopCore :: (Show l) => LHE.QOp l -> ParseM Term
qopCore (LHE.QVarOp _l qn) = qNameCore qn
qopCore (LHE.QConOp _l qn) = qNameCore qn

literalCore :: (Show l) => LHE.Literal l -> ParseM Term
literalCore (LHE.Int _l i s) = fmap var $ intWrapper i
literalCore (LHE.Char _l c s) = fmap var $ charWrapper c
literalCore (LHE.String _l s _s) = mapM (literalCore . (\c -> LHE.Char c c _s)) s >>= list

altCore :: (Show l) => LHE.Alt l -> ParseM Alt
altCore (LHE.Alt _loc pat (LHE.UnGuardedRhs _l e) (Just (LHE.BDecls _l' binds))) = do
    xes <- declsCore binds
    e <- expCore e
    return (altcon, build (bind xes e))
  where (altcon, build) = altPatCore pat
altCore (LHE.Alt _loc pat (LHE.UnGuardedRhs _l e) Nothing) =
    altCore (LHE.Alt _loc pat (LHE.UnGuardedRhs _l e) (Just (LHE.BDecls _l [])))

altPatCore :: (Show l) => LHE.Pat l -> (AltCon, Term -> Term)
altPatCore (LHE.PApp _l qname pats)           = dataAlt (qNameDataCon qname) (patCores pats)
altPatCore (LHE.PInfixApp _l pat1 qname pat2) = dataAlt (qNameDataCon qname) (patCores [pat1, pat2])
altPatCore (LHE.PTuple _l _b [pat1, pat2])       = dataAlt pairDataCon (patCores [pat1, pat2])
altPatCore (LHE.PParen _l pat)                = altPatCore pat
altPatCore (LHE.PList _l [])                  = dataAlt nilDataCon ([], [], id)
altPatCore (LHE.PLit _l _sign (LHE.Int _l' i _s))          = (LiteralAlt (Int i), id)
altPatCore (LHE.PWildCard _l)                   = (DefaultAlt Nothing, id)
altPatCore p = panic "altPatCore" (text $ show p)

dataAlt :: DataCon -> ([Var], [Var], Term -> Term) -> (AltCon, Term -> Term)
dataAlt dcon (names, _bound_ns, build) = (DataAlt dcon names, build)


specialConDataCon :: LHE.SpecialCon l -> DataCon
specialConDataCon (LHE.UnitCon _l) = unitDataCon
specialConDataCon (LHE.ListCon _l) = nilDataCon
specialConDataCon (LHE.TupleCon _l LHE.Boxed 2) = pairDataCon
specialConDataCon (LHE.Cons _l) = consDataCon

nameString :: LHE.Name l -> String
nameString (LHE.Ident _l s)  = s
nameString (LHE.Symbol _l s) = s

qNameCore :: (Show l) => LHE.QName l -> ParseM Term
qNameCore (LHE.UnQual _l n) = fmap var $ case nameString n of
    "+"   -> primWrapper Add
    "-"   -> primWrapper Subtract
    "*"   -> primWrapper Multiply
    "div" -> primWrapper Divide
    "mod" -> primWrapper Modulo
    "=="  -> primWrapper Equal
    "<"   -> primWrapper LessThan
    "<="  -> primWrapper LessThanEqual
    s -> return (name s)
qNameCore (LHE.Special _l sc) = fmap var $ dataConWrapper $ specialConDataCon sc
qNameCore qn = panic "qNameCore" (text $ show qn)

qNameDataCon :: LHE.QName l -> DataCon
qNameDataCon (LHE.UnQual _l name) = nameString name
qNameDataCon (LHE.Special _l sc)  = specialConDataCon sc

patCores :: (Show l) => [LHE.Pat l] -> ([Var], [Var], Term -> Term)
patCores []     = ([], [], id)
patCores (p:ps) = (n':ns', bound_ns' ++ bound_nss', build . build')
  where (n', bound_ns', build) = patCore p
        (ns', bound_nss', build') = patCores ps

-- TODO: this function is a hilarious shadowing bug waiting to happen. Thread the IdSupply in here to generate temp names.
patCore :: (Show l) =>
        LHE.Pat l        -- Pattern
        -> (Var,          -- Name consumed by the pattern
            [Var],        -- Names bound by the pattern
            Term -> Term) -- How to build the (strict) consuming context around the thing inside the pattern
patCore (LHE.PVar _l n)    = (x, [x], id)
  where x = name (nameString n)
patCore (LHE.PWildCard _l)   = (x, [x], id)
  where x = name "_"
patCore (LHE.PParen _l p)  = patCore p
patCore (LHE.PTuple _l _b ps) = case tupleDataCon (length ps) of
    Nothing | [p] <- ps -> patCore p
    Just dc -> (n', bound_ns', \e -> case_ (var n') [(DataAlt dc ns', build e)])
      where n' = name "tup"
            (ns', bound_ns', build) = patCores ps
patCore (LHE.PInfixApp _l p1 qinfix p2) = (n', bound_ns1 ++ bound_ns2, \e -> case_ (var n') [(DataAlt (qNameDataCon qinfix) [n1', n2'], build1 (build2 e))])
  where n' = name "infx"
        (n1', bound_ns1, build1) = patCore p1
        (n2', bound_ns2, build2) = patCore p2
patCore (LHE.PApp _l (LHE.Special _l' (LHE.UnitCon _l'')) []) = (name "unit", [], id)
patCore p = panic "patCore" (text $ show p)

bind :: [(Var, Term)] -> Term -> Term
bind xes e = letRec xes e
