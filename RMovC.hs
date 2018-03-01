{-# LANGUAGE ViewPatterns, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module RMovC where
import Text.Show.Prettyprint
import Control.Monad.RWS.Strict
import Control.Monad.Except
import Control.Arrow
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map.Strict(Map)
import Data.Set(Set)
import Data.List
import Data.Maybe
import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P
import Text.Parsec.Perm

import System.Environment
import System.Exit

-- TODO input validation: data dependencies, linear typing
-- TODO automatic placement of dividers

-- Main

data Op = OBd | OInstrs | OCpp | ODot | OInv

main :: IO ()
main = do
  args <- getArgs
  (fn, bn, op) <-
    case args of
      [f] -> return (f, Nothing, Nothing)
      [f, b] -> return (f, Just b, Nothing)
      [f, b, ""] -> return (f, Just b, Nothing)
      [f, b, "bd"] -> return (f, Just b, Just OBd)
      [f, b, "instrs"] -> return (f, Just b, Just OInstrs)
      [f, b, "cpp"] -> return (f, Just b, Just OCpp)
      [f, b, "dot"] -> return (f, Just b, Just ODot)
      [f, b, "inv"] -> return (f, Just b, Just OInv)

      _ -> do
        p <- getProgName
        putStrLn $ p ++ " FILENAME [BLOCK [bd|instrs|cpp|dot] | -]"
        exitWith $ ExitFailure 1

  bds <- parseBDs <$> readFile "Example.bd"

  case bn of
    Nothing -> do
      print' bds
      exitSuccess

    Just "-" -> do
      print' $ M.keys bds
      exitSuccess

    Just b -> do
      let inScope = ["IN"]
      let em = fromRight (runEmitFromMap (inScope, "") b bds)
      let ((outs, d), eS, instrs) = em
      let bd = bds M.! b

      case op of
        Nothing -> printRes (inScope, bd) em
        Just OBd -> print' bd
        Just OInstrs -> putStrLn $ dispTree dtITree $ toITree instrs
        Just OCpp -> putStrLn $ instrsToCpp (inScope, outs, bd) instrs
        Just ODot -> putStrLn $ instrsToGraphviz instrs
        Just OInv -> do
          let bd' = invBD bd
          let bd'' = invBD bd'
          print' bd'
          putStrLn $ "consistent inverse: " ++ show (bd == bd'')

  return ()

----------------------------

-- Samples

emitMovs0, emitMux0, emitMux20, emitRev30, emitPerm50
  :: Either EmitE RunEmitRes

emitMovs, emitMux, emitMux2, emitRev3, emitPerm5
  :: RunEmitRes

movs :: BlockDef
movs = BD { bdIn = ["a", "b", "c", "d", "e", "f"]
          , bdOut = ["u", "v", "w", "x", "y", "z"]
          , bdInt = set ["a1", "b1", "b2", "c1", "e1"]
          , bdBody = [ [ (BI, AP ["b1", "a1"] ["a", "b"])
                       , (BI, AP ["e1"] ["e"]) ]
                     , [ (BI, AP ["c1", "b2"] ["e1", "c"]) ]
                     , [ (BI, AP ["v", "w", "x", "y", "z"]
                                 ["d", "c1", "a1", "b1", "b2"])
                     , (BI, AP ["u"] ["f"]) ] ] }

emitMovs0 = runEmit ctx emptyEmitS $ emitBD "M" movs where
  ctx = ctxFromBD ["IN"] "i" movs

emitMovs = fromRight emitMovs0

mux :: BlockDef
mux = BD { bdIn = ["m.0", "m.1", "x.0", "x.1"]
         , bdOut = ["a.0", "a.1", "b.0", "b.1", "ma.0", "ma.1"]
         , bdInt = set ["r"]
         , bdBody = [ [ (BT, AP ["r", "a.0", "b.0"] ["m.1", "x.0"]) ]
                    , [ (BT, AP ["ma.1", "a.1", "b.1"] ["r", "x.1"])
                      , (BI, AP ["ma.0"] ["m.0"]) ] ] }

emitMux0 = runEmit ctx emptyEmitS $ emitBD "M" mux where
  ctx = ctxFromBD ["IN"] "i" mux

emitMux = fromRight emitMux0

mux2 :: BlockDef
mux2 = BD { bdIn = ["m.0", "m.1", "x.0", "x.1", "y.0", "y.1"]
          , bdOut = [ "a.0", "a.1", "b.0", "b.1", "c.0", "c.1"
                    , "d.0", "d.1", "mb.0", "mb.1" ]
          , bdInt = set ["ma.0", "ma.1"]
          , bdBody = [ [ (BN "muxa" "mux"
                         , AP ["a.0", "a.1", "b.0", "b.1", "ma.0", "ma.1"]
                              ["m.0", "m.1", "x.0", "x.1"]) ]
                     , [ (BN "muxb" "mux"
                         , AP ["c.0", "c.1", "d.0", "d.1", "mb.0", "mb.1"]
                              ["ma.0", "ma.1", "y.0", "y.1"]) ] ] }

emitMux20 = runEmit ctx emptyEmitS $ emitBD "M2" mux2 where
  ctx0 = ctxFromBD ["IN"] "i" mux2
  ctx = ctx0 { ecBDs = mp [("mux", mux)] }

emitMux2 = fromRight emitMux20

rev2 :: BlockDef
rev2 = BD { bdIn = ["x.0", "x.1"]
          , bdOut = ["a.0", "a.1"]
          , bdInt = S.empty
          , bdBody = [ [ (BI, AP ["a.1", "a.0"] ["x.0", "x.1"]) ] ]}

rev3 :: BlockDef
--rev3 = BD { bdIn = ["x", "y", "z"]
--          , bdOut = ["a", "b", "c"]
--          , bdInt = set ["r", "s", "t"]
--          , bdBody = [ [ (BI, AP ["r", "s"] ["x", "y"]) ]
--                     , [ (BI, AP ["t", "c"] ["s", "z"]) ]
--                     , [ (BI, AP ["a", "b"] ["r", "t"]) ] ]}

rev3 = BD { bdIn = ["x", "y", "z"]
          , bdOut = ["a", "b", "c"]
          , bdInt = set ["r", "s", "t"]
          , bdBody = [ [ (BN "r2a" "rev2", AP ["r", "s"] ["x", "y"]) ]
                     , [ (BN "r2b" "rev2", AP ["t", "c"] ["s", "z"]) ]
                     , [ (BN "r2c" "rev2", AP ["a", "b"] ["r", "t"]) ] ]}

emitRev30 = runEmit ctx emptyEmitS $ emitBD "R3" rev3 where
  ctx0 = ctxFromBD ["IN"] "i" rev3
  ctx = ctx0 { ecBDs = mp [("rev2", rev2)] }

emitRev3 = fromRight emitRev30

perm5 :: BlockDef
perm5 = BD { bdIn = ["v", "w", "x", "y", "z"]
           , bdOut = ["a", "b", "c", "d", "e"]
           , bdInt = set ["s"]
           , bdBody = [ [ (BN "r3a" "rev3", AP ["b", "a", "s"] ["v", "w", "x"]) ]
                      , [ (BN "r3b" "rev3", AP ["d", "e", "c"] ["s", "y", "z"]) ] ] }

emitPerm50 = runEmit ctx emptyEmitS $ emitBD "P5" perm5 where
  ctx0 = ctxFromBD ["IN"] "i" perm5
  ctx = ctx0 { ecBDs = mp [("rev2", rev2), ("rev3", rev3)] }

emitPerm5 = fromRight emitPerm50

--
--  x  y  z
--  |  |  :
--  y  x  :  <--  r  s
--  :  |  |
--  :  z  x  <--     t
--  |  |  :
--  z  y  x
--

runEmitFromMap :: (Scope, String)
                    -> String -> Map String BlockDef
                    -> Either EmitE RunEmitRes

runEmitFromMap (inScope, inPrefix) name bds =
    runEmit ctx emptyEmitS $ emitBD name b where
  b = bds M.! name
  ctx0 = ctxFromBD inScope inPrefix b
  ctx = ctx0 { ecBDs = M.delete name bds }

fromRight :: Show a => Either a b -> b
fromRight (Left x) = error $ "fromRight: " ++ show' x
fromRight (Right x) = x

show' :: Show a => a -> String
show' = prettyShow

print' :: Show a => a -> IO ()
print' = putStrLn . show'

printRes :: (Scope, BlockDef) -> RunEmitRes -> IO ()
printRes (inScope, bd) ((outs, d), eS, instrs) = do
  print' outs
  print' d
  print' eS
  putStrLn "------"
  putStrLn $ dispTree dtITree $ toITree instrs
  putStrLn "------"
  putStrLn $ instrsToCpp (inScope, outs, bd) instrs
  putStrLn "------"
  putStrLn $ instrsToGraphviz instrs

set :: Ord a => [a] -> Set a
set = S.fromList

mp :: Ord k => [(k, v)] -> Map k v
mp = M.fromList

ctxFromBD :: Scope -> String -> BlockDef -> EmitCtx
ctxFromBD scope pre BD { bdIn, bdOut } =
  EC { ecIns = mp $ map (\n -> (n, A Nothing scope $ pre ++ n)) bdIn
     , ecOuts = bdOut
     , ecScope = []
     , ecBDs = M.empty }

----------------------------

lang :: P.LanguageDef st
lang =
  emptyDef
    { P.identStart      = letter <|> char '_'
    , P.identLetter     = alphaNum <|> char '_'
    , P.reservedOpNames = ["=", "~", ".", "*", ":", "<-"]
    , P.reservedNames   = ["in", "out", "int", "body", "T"]
    , P.caseSensitive   = True }

type Parser u a = ParsecT String u Data.Functor.Identity.Identity a

data Suffix = SN String | SStar Integer

data Part a b c d = PIn a | POut b | PInt c | PBody d

lexer :: P.TokenParser st
lexer = P.makeTokenParser lang

sepList :: [Maybe a] -> [[a]]
sepList = sepList' [] [] where
  sepList' xs ys [] = xs ++ [ys]
  sepList' xs ys (Nothing:zs) = sepList' (xs ++ [ys]) [] zs
  sepList' xs ys (Just y:zs) = sepList' xs (ys ++ [y])  zs

bd :: Parser u (String, BlockDef)
bd = blk where

  blk = (,) <$> ident <*> (bd <$> braces parts)

  parts = permute (c4 <$?> (te, t4 <$> bodyPart)
                      <|?> (te, t1 <$> inPart)
                      <|?> (te, t2 <$> outPart)
                      <|?> (te, t3 <$> intPart) )

  bd (x, y, i, b) = BD { bdIn = x, bdOut = y, bdInt = S.fromList i, bdBody = b }
  c4 a b c d = a <> b <> c <> d
  te = ([], [], [], [])
  t1 x = (x, [], [], [])
  t2 x = ([], x, [], [])
  t3 x = ([], [], x, [])
  t4 x = ([], [], [], x)

  inPart = inKw *> braces names
  outPart = outKw *> braces names
  intPart = intKw *> braces names
  bodyPart = bodyKw *> braces stmts

  ------------

  movPart = arrow *> names
  funcPart = equals *> ((,) <$> funcName <*> parens names)
  funcName :: Parser u BName
  funcName = try (BN <$> ident <*> (colon *> ident))
             <|> (BNI <$> ident <*> (colontilde *> ident))
             <|> (const BTI <$> (tilde *> tKw))
             <|> (const BT <$> tKw)

  bnPart = do
    biName <- ident
    _ <- colon
    f <- (tilde *> (flip BNI <$> ident)) <|> (flip BN <$> ident)
    return $ f biName


  assignStmt = getStmt <$> names <*> (fmap Left movPart <|> fmap Right funcPart)

  stmts = filter (not . null) <$> sepEndBy (sepEndBy assignStmt semi) divider

  getStmt :: [String] -> Either [String] (BName, [String])
                      -> (BName, ArgsPair)
  getStmt ys (Left xs) = (BI, AP ys xs)
  getStmt ys (Right (n, xs)) = (n, AP ys xs)

  divider = (many1 (char '-') >> ws >> optional semi) <?> "divider"

  ------------

  suffix :: Parser u Suffix
  suffix = (dot *> (SN <$> (ident <|> show <$> num)))
           <|> (const (SStar 1) <$> star)
  nameElem :: Parser u [String]
  nameElem = applySuffix <$> ident <*> optionMaybe suffix

  names :: Parser u [String]
  names = concat <$> commaSep (bsPart <|> nameElem)

  bsPart = applySuffix' <$> squares (commaSep nameElem) <*> suffix

  applySuffix' :: [[String]] -> Suffix -> [String]
  applySuffix' xs s = concatMap (concatMap $ flip applySuffix $ Just s) xs

  applySuffix :: String -> Maybe Suffix -> [String]
  applySuffix x Nothing = [x]
  applySuffix x (Just (SN s)) = [x ++ "." ++ s]
  applySuffix x (Just (SStar n)) = [x ++ "." ++ show n | n <- [0..n]]

  ------------

  braces = P.braces lexer
  squares = P.squares lexer
  parens = P.parens lexer
  ident = P.identifier lexer
  num = P.natural lexer
  commaSep = P.commaSep lexer

  colontilde = try (colon *> tilde) <|> P.reservedOp lexer ":~"
  arrow = P.reservedOp lexer "<-"
  tilde = P.reservedOp lexer "~"
  equals = P.reservedOp lexer "="
  dot = P.reservedOp lexer "."
  star = P.reservedOp lexer "*"
  semi = P.reservedOp lexer ";"
  colon = P.reservedOp lexer ":"

  ws = P.whiteSpace lexer

  inKw = P.reserved lexer "in"
  outKw = P.reserved lexer "out"
  intKw = P.reserved lexer "int"
  bodyKw = P.reserved lexer "body"
  tKw = P.reserved lexer "T"

parseBD :: String -> (String, BlockDef)
parseBDs :: String -> Map String BlockDef
parseBD = either (error . show) id . parse (bd <* eof) "<parseBD>"
parseBDs = either (error . show) M.fromList . parse (many bd <* eof) "<parseBDs>"


------------------------------

type AName = String

type BIName = String

type FName = String

type Args = [AName]

data BName = BI | BT | BTI | BN BIName FName | BNI BIName FName
  deriving (Eq, Ord, Show, Read)

data ArgsPair = AP { apOut :: Args, apIn :: Args }
  deriving (Eq, Ord, Show, Read)

type Stmt = (BName, ArgsPair)
type Level = [Stmt]

data BlockDef = BD { bdIn :: [AName]
                   , bdOut :: [AName]
                   , bdInt :: Set AName -- TODO never checked
                   , bdBody :: [Level] }
  deriving (Eq, Ord, Show, Read)

data LBlockInfo = LBI { lbiBIName :: String
                      , lbiScope :: Scope
                      , lbiIns :: Map AName Addr
                      , lbiOuts :: Map AName Addr }
  deriving (Eq, Ord, Show, Read)

data Label = LEmpty | LLevel Delay Delay | LBlock LBlockInfo
  deriving (Eq, Ord, Show, Read)

type Scope = [String]

data Addr = A (Maybe Delay) Scope String
  deriving (Eq, Ord, Read)

data Instr = IBeginGroup
           | IEndGroup Label
           | IDelay Delay (Delay, Delay) Addr Addr
           | IMov Addr Addr
           | ICMov Bool Addr Addr Addr Addr Addr
  deriving (Eq, Ord, Show, Read)

data Tree b l = Leaf l | Node b [Tree b l]
  deriving (Eq, Ord, Show, Read)

data DispTree b l = DT { dtOpen :: b -> String
                       , dtL :: l -> String
                       , dtI :: Int -> String
                       , dtClose :: String }

type ITree = Tree Label Instr

type STree = Tree String String

type Delay = Integer

data EmitCtx = EC { ecIns :: Map AName Addr
                  , ecOuts :: [AName]
                  , ecScope :: Scope
                  , ecBDs :: Map FName BlockDef }
  deriving (Eq, Ord, Show, Read)

data EmitErr = BDNotFound FName
             | BIArityErr BName
             | VarNotFound AName
             | VarAlreadyUsed AName
             | OutstandingVars [AName]
  deriving (Eq, Ord, Show, Read)

data VarInfo = VI { viUsed :: Bool, viDelay :: Delay, viAddr :: Addr }
  deriving (Eq, Ord, Show, Read)

type EmitW = [Instr]
data EmitS = EmitS { esVars :: Map AName VarInfo
                   , esCurDelay :: Delay }
  deriving (Eq, Ord, Show, Read)

data EmitE = EmitE { eeScope :: Scope
                   , eeDelay :: Delay
                   , eeVars :: Set AName
                   , eeErr :: EmitErr }
  deriving (Eq, Ord, Show, Read)

newtype Emit a = Emit { getEmit :: RWST EmitCtx EmitW EmitS (Except EmitE) a }
  deriving (Functor, Applicative, Monad, MonadReader EmitCtx,
            MonadWriter EmitW, MonadState EmitS, MonadError EmitE)

type RunEmitRes = ((Map AName Addr, Delay), EmitS, EmitW)

------------------------------

dispScope :: Scope -> String
dispScope = intercalate "."

dispLabel :: Label -> String
dispLabel LEmpty = "LEmpty"
dispLabel (LLevel a b) = "Level: " ++ show a ++ " -> " ++ show b
dispLabel (LBlock LBI { lbiBIName, lbiScope, lbiIns, lbiOuts }) =
  "LBI: " ++ lbiBIName ++ " (" ++ ins'
          ++ ") -> (" ++ outs' ++ ") @ " ++ dispScope lbiScope where
  ins' = intercalate ", " $ map (dispAddr . snd) $ M.toList lbiIns
  outs' = intercalate ", " $ map (dispAddr . snd) $ M.toList lbiOuts

dispAddr :: Addr -> String
dispAddr (A dd scope n) = "(" ++ sp ++ n ++ dp ++ ")" where
  dp = maybe "" (("#" ++) . show) dd
  sp = if null scope then ":" else dispScope scope ++ ":"

instance Show Addr where
  show a = "a " ++ show (dispAddr a)

arrow :: String
arrow = " -> "

dispInstr :: Instr -> String
dispInstr IBeginGroup = "# {"
dispInstr (IEndGroup (LLevel d d1)) = "# } " ++ show (d, d1)
dispInstr (IEndGroup l) = "# }"

dispInstr (IMov a b) = "mov\t" ++ dispAddr a ++ arrow ++ dispAddr b
dispInstr (IDelay i p a b) =
    "delay\t" ++ show i ++ " " ++ show p ++ " "
    ++ dispAddr a ++ arrow ++ dispAddr b

dispInstr (ICMov False a b c d e) =
    "cmov\t" ++ dispAddr a ++ " " ++ dispAddr b
    ++ arrow ++ dispAddr c ++ " " ++ dispAddr d ++ " " ++ dispAddr e

dispInstr (ICMov True a b c d e) =
    "rcmov\t" ++ dispAddr c ++ " " ++ dispAddr d ++ dispAddr e
    ++ arrow ++ dispAddr a ++ " " ++ dispAddr b

data I2GS = I2GS { i2gsIndent :: Integer, i2gsCounter :: Integer }
  deriving (Eq, Ord, Show, Read)

type I2G a = RWS () [String] I2GS a

instrsToGraphviz :: [Instr] -> String
instrsToGraphviz is = dispTree dtString t where
  (t, _, r) = runRWS m () (I2GS 1 0)
  m = do
    t <- iTreeToGraphviz $ toITree is
    return $ Node "digraph G" [Leaf "node[shape=Mrecord];", t]

a2g :: Addr -> String
a2g = show . dispAddr

iTreeToGraphviz :: ITree -> I2G STree
iTreeToGraphviz (Node (LBlock LBI { lbiBIName
                                  , lbiScope
                                  , lbiIns
                                  , lbiOuts }) xs) = do
  let (inAddrs, outAddrs) = (snd <$> M.toList lbiIns, snd <$> M.toList lbiOuts)
  let groupBy key = M.fromListWith (++) . map (key &&& pure)
  let isRoot = length lbiScope < 2

  let m = travInstrs (M.fromList $ map (flip (,) 0) inAddrs)
            $ concatMap subInstrs xs
  let mg = groupBy snd $ M.toList m
  let (maxDepth, _) = M.findMax mg
  let ad = M.map (map fst) $ M.filterWithKey (\k _ -> 0 < k && k < maxDepth) mg

  let ds0 = map (\(_, xs)
                 -> Node "" (Leaf "rank=same;"
                            : map (Leaf . (++ ";") . a2g) xs)) $ M.toList ad
  let ds = if isRoot then ds0 else []
  --let ds = []

  let biName = intercalate "_" lbiScope
  let dispName = dispScope lbiScope

  let mv = map (\(v, dispAddr -> a) ->
                  Leaf (show a ++ "[label=" ++ show (v ++ "=" ++ a) ++ "]")) . M.toList

  let c1 = "subgraph cluster_" ++ biName ++ "_in"
  let n1 = Node c1 (Leaf "label=in;" : mv lbiIns)

  let c2 = "subgraph cluster_" ++ biName ++ "_out"
  let n2 = Node c2 (Leaf "label=out;" : mv lbiOuts)

  let n3 = Leaf $ "label=" ++ show dispName ++ ";"

  let pre = if isRoot then [n1, n2, n3] else [n3]
  xs' <- mapM iTreeToGraphviz xs
  return $ Node ("subgraph " ++ (if isRoot then "cluster_" else "_") ++ biName)
                (pre ++ ds ++ [Leaf ""] ++ xs')

iTreeToGraphviz (Node b xs) = do
  xs' <- mapM iTreeToGraphviz xs
  name <- gName
  return $ Node ("subgraph " ++ name) xs'

iTreeToGraphviz (Leaf (IDelay d p a0@(a2g -> a) b0@(a2g -> b))) =
  return $ Leaf (a ++ " -> " ++ b ++ " [label=\"d"
                 ++ show d ++ " " ++ show p ++ "\"];")

iTreeToGraphviz (Leaf (IMov a0@(a2g -> a) b0@(a2g -> b))) =
  return $ Leaf (a ++ " -> " ++ b ++ ";")

iTreeToGraphviz (Leaf (ICMov False a b c d e)) =
  Node "" <$> cmovToGraphviz False [a, b] [c, d, e]

iTreeToGraphviz (Leaf (ICMov True a b c d e)) =
  Node "" <$> cmovToGraphviz True [c, d, e] [a, b]

iTreeToGraphviz (Leaf _) = return $ Node "" []

cmovToGraphviz :: Bool -> [Addr] -> [Addr] -> I2G [STree]
cmovToGraphviz r ins outs = do
  c <- i2gCounter

  let name = (if r then "r" else "") ++ "cmov" ++ show c
  let styles = " [shape=rect, label=\"\", height=0.1, margin=0,"
               ++ " color=\"#444444\", style=filled];"
  let l1 = Leaf (name ++ styles)
  let a x y z = x ++ " -> " ++ y ++ " " ++ z ++ ";"

  let as = map (\(a2g -> n, l) ->
                   Leaf $ a n name ("[arrowhead=none, label=" ++ l ++ "]"))
               $ zip ins ["s", "a", "b"]
  let bs = map (\(a2g -> n, l) -> Leaf $ a name n (" [label=" ++ l ++ "]"))
               $ zip outs ["s", "a", "b"]
  return $ [l1] ++ as ++ bs

i2gCounter :: I2G Integer
i2gCounter = do
  s@I2GS { i2gsCounter = c } <- get
  put s { i2gsCounter = c + 1 }
  return c

gName :: I2G String
gName = do
  c <- i2gCounter
  return $ "g" ++ show c

toITree :: [Instr] -> ITree
toITree = Node LEmpty . toITree' [] []

toITree' :: [[ITree]] -> [ITree] -> [Instr] -> [ITree]
toITree' [] ns [] = ns

toITree' stk ns []    = error "toITree': Not enough closes."
toITree' stk ns (IBeginGroup : is) = toITree' (ns : stk) [] is

toITree' (ns0:stk) ns (IEndGroup v : is) = toITree' stk (ns0 ++ [Node v ns]) is
toITree' [] _ (IEndGroup _ : is) = error "toITree': too many closes."

toITree' stk ns (i:is) = toITree' stk (ns ++ [Leaf i]) is

indent2, indent4 :: Int -> String
indent2 = flip replicate ' ' . (2*)
indent4 = flip replicate ' ' . (4*)

bbrace :: String -> String
bbrace "" = "{"
bbrace s = s ++ " {"

dtString :: DispTree String String
dtString = DT bbrace id indent2 "}"

dtCpp :: DispTree String String
dtCpp = DT bbrace id indent2 "};"

dtShow :: (Show b, Show l) => DispTree b l
dtShow = DT (bbrace . show) show indent2 "}"

dtITree :: DispTree Label Instr
dtITree = DT (bbrace . dispLabel) dispInstr indent2 "}"

dispTree :: DispTree b l  -> Tree b l -> String
dispTree dt t = dispTree' dt ([], 0) [Just t]

dispTree' :: DispTree b l -> ([String], Int) -> [Maybe (Tree b l)] -> String
dispTree' dt (s, _) [] = unlines s
dispTree' dt@DT { dtI, dtL } (s, i) (Just (Leaf l) : ts) =
  dispTree' dt (s ++ [dtI i ++ dtL l], i) ts

dispTree' dt@DT { dtI, dtOpen, dtL } (s, i) (Just (Node b xs) : ts) =
  dispTree' dt (s ++ [dtI i ++ dtOpen b], i + 1) (map Just xs ++ [Nothing] ++ ts)

dispTree' dt@DT { dtI, dtClose } (s, i) (Nothing : ts) =
  dispTree' dt (s ++ [dtI (i - 1) ++ dtClose], i - 1) ts

subInstrs :: ITree -> [Instr]
subInstrs (Leaf i) = [i]
subInstrs (Node _ xs) = concatMap subInstrs xs

addrs :: Instr -> ([Addr], [Addr])
addrs (IDelay _ _ a b) = ([a], [b])
addrs (IMov a b) = ([a], [b])
addrs (ICMov False a b c d e) = ([a, b], [c, d, e])
addrs (ICMov True a b c d e) = ([c, d, e], [a, b])
addrs _ = ([], [])

travInstrs :: Map Addr Delay -> [Instr] -> Map Addr Delay
travInstrs m [] = m
travInstrs m (i:is) = travInstrs m' is where
  (ins, outs) = addrs i
  ds = nub $ concatMap (maybe [] pure . (`M.lookup` m)) ins
  d = case ds of
         [d] -> d
         _ -> error $ "travInstrs: bad depth: "
                       ++ show (ds, ins) ++ "\n" ++ show' m

  d' = d + 1
  m' = foldl' (\m a -> M.insert a d' m) m outs

--------------------------------

sortByIdx :: (Ord a, Ord b) => (a -> b, [b], Bool) -> [a] -> [a]
sortByIdx (f, xs, k) = map snd . sort . map f1 where
  f1 a@(f -> n) = (fromMaybe m $ lookup n xs', a)
  xs' = zip xs ([0..] :: [Integer])
  m = if k then 1 + fromIntegral (length xs) else -1

instrsToCpp :: (Scope, Map AName Addr, BlockDef) -> [Instr] -> String
instrsToCpp s@(inScope, outs, BD { bdIn, bdOut }) instrs =
             dispTree dtCpp vIn ++ "\n"
          ++ dispTree dtCpp vOut ++ "\n"
          ++ dispTree dtCpp vp ++ "\n"
          ++ dispTree dtCpp vI ++ "\n"
          where

  ias = indices instrs
  mi = M.fromList $ map (\(a, b) -> (b, a)) ias
  getI = (mi M.!)

  vp = cppVec' "string" "addr_names"
         $ map (\(b, a) -> show (dispAddr a) ++ ", \t// " ++ show b) ias
  vI = cppVec "instr" "instrs" $ map instrToCpp $ filter isEssential instrs

  vIn = cppVec' "int" "in_idx" $ map (nLine (`lookup` inAddrs)) ins'
  vOut = cppVec' "int" "out_idx" $ map (nLine (`M.lookup` outs)) outs'

  nLine lookup (n, i) = show (getI $ fromJust $ lookup n) ++ ", \t// " ++ show n

  inAddrs :: [(AName, Addr)]
  inAddrs = map (\(_, a, s) -> (s, a))
              $ sort $ concatMap ((\a@(A _ sc s) -> [ (lookup s ins', a, s)
                                                    | sc == inScope ]) . snd) ias

  fromZ :: [Integer]
  fromZ = [0..]
  ins' = zip bdIn fromZ
  outs' = zip bdOut fromZ

  isEssential IBeginGroup = False
  isEssential (IEndGroup _) = False
  isEssential _ = True

  instrToCpp :: Instr -> String
  gi = show . getI
  instrToCpp (IMov (gi -> a) (gi -> b)) = "mov(" ++ a ++ ", " ++ b ++")"
  instrToCpp (ICMov isR (gi -> a) (gi -> b) (gi -> c) (gi -> d) (gi -> e)) =
    (if isR then "r" else "") ++ "cmov(" ++ intercalate ", " [a, b, c, d, e] ++")"
  instrToCpp _ = error "instrToCpp"

indices :: [Instr] -> [(Integer, Addr)]
indices = zip [0..] . nub . concatMap (uncurry (++) . addrs)

cppVec, cppVec' :: String -> String -> [String] -> STree
cppVec t n (map (Leaf . (++ ",")) -> xs) = Node ("vector<" ++ t ++ "> " ++ n) xs
cppVec' t n (map Leaf -> xs) = Node ("vector<" ++ t ++ "> " ++ n) xs

--------------------------------

invBD :: BlockDef -> BlockDef
invBD BD { bdIn, bdOut, bdInt, bdBody } =
  BD { bdIn = bdOut, bdOut = bdIn, bdInt, bdBody = invBody bdBody }

invBody :: [Level] -> [Level]
invBody = reverse . map (reverse . map invStmt)

invStmt :: Stmt -> Stmt
invStmt (bn, AP o i) = (invBName bn, AP i o)

invBName :: BName -> BName
invBName BI = BI
invBName BT = BTI
invBName BTI = BT
invBName (BN n fn) = BNI n fn
invBName (BNI n fn) = BN n fn

--------------------------------

emitInstrs :: [Instr] -> Emit ()
emitInstrs = tell

emptyCtx :: EmitCtx
emptyCtx = EC M.empty [] [] M.empty

runEmit :: EmitCtx -> EmitS -> Emit a -> Either EmitE (a, EmitS, EmitW)
runEmit c s (getEmit -> e) = runExcept $ runRWST e c s

emptyEmitS :: EmitS
emptyEmitS = EmitS M.empty 0

emitErr :: EmitErr -> Emit a
emitErr err = do
  s <- ecScope <$> ask
  EmitS { esCurDelay, esVars } <- get
  throwError $ EmitE s esCurDelay (M.keysSet esVars) err

withState :: MonadState s m => s -> m a -> m a
withState s' m = do
  s <- get
  put s'
  r <- m
  put s
  return r

getScope :: Emit Scope
getScope = ecScope <$> ask

getCurDelay :: Emit Delay
getCurDelay = esCurDelay <$> get

setCurDelay :: Delay -> Emit ()
setCurDelay d' = get >>= \es -> put es { esCurDelay = d' }

insertVarInfo :: AName -> VarInfo -> Emit ()
insertVarInfo name v = do
  es@EmitS { esVars } <- get
  put es { esVars = M.insert name v esVars }

getVarInfo :: AName -> Emit VarInfo
getVarInfo name = do
  es@EmitS { esVars } <- get
  case M.lookup name esVars of
    Nothing -> emitErr $ VarNotFound name
    Just v -> return v

getUnusedVarInfo :: AName -> Emit (Delay, Addr)
getUnusedVarInfo name = do
  VI { viUsed, viDelay, viAddr } <- getVarInfo name
  when viUsed $ emitErr (VarAlreadyUsed name)
  return (viDelay, viAddr)

newAddrD :: Delay -> AName -> Emit Addr
newAddrD dd name = do
  s <- getScope
  return $ A (Just dd) s name

newAddr :: AName -> Emit Addr
newAddr name = do
  EC { ecScope, ecIns, ecOuts } <- ask
  return (A Nothing ecScope name)

getAddr :: AName -> Emit Addr
getAddr = fmap viAddr . getVarInfo

group :: Label -> Emit a -> Emit a
group l = group' (return l)

group' :: Emit Label -> Emit a -> Emit a
group' getL m = do
  emitInstrs [IBeginGroup]
  r <- m
  l <- getL
  emitInstrs [IEndGroup l]
  return r

emitBD :: String -> BlockDef -> Emit (Map AName Addr, Delay)
emitBD instName BD { bdBody } =
  local (addScope instName) $ do
    EC { ecIns } <- ask
    d <- getCurDelay
    let s' = EmitS (M.map (VI False d) ecIns) d

    withState s' $ do
      EC { ecIns, ecOuts, ecScope } <- ask

      let getL = do outs' <- mapM getAddr ecOuts
                    let lbi = LBI instName ecScope ecIns
                                (M.fromList $ zip ecOuts outs')
                    return $ LBlock lbi

      group' getL $ do
        mapM_ emitLevel bdBody

        EmitS { esCurDelay = d } <- get
        mapM_ (advanceTo False d) ecOuts

        es@EmitS { esVars, esCurDelay } <- get

        let unusedVars = M.filter (not . viUsed) esVars
        let outs = S.fromList ecOuts
        let diff = S.difference outs (M.keysSet unusedVars)
        unless (S.null diff) $ emitErr (OutstandingVars $ S.toList diff)

        let outVars = M.map viAddr
                       $ M.filterWithKey (flip $ const (`S.member` outs)) esVars

        return (outVars, esCurDelay)

addScope :: String -> EmitCtx -> EmitCtx
addScope instName ec@EC { ecScope } = ec { ecScope = ecScope ++ [instName] }

emitLevel :: Level -> Emit ()
emitLevel lv = do
  d0 <- getCurDelay
  group' (LLevel d0 <$> getCurDelay) $ do
    d0 <- getCurDelay
    ds <- mapM emitSingle lv
    setCurDelay $ maximum ds

delayVar :: Delay -> Delay -> AName -> Emit Addr
delayVar d0 d1 name
  | d1 - d0 < 0 = error $ "delayVar " ++ show d0 ++ " " ++ show d1
                          ++ " " ++ show name ++ ": negative"
  | d1 == d0 = getAddr name

delayVar d0 d1 name = do
  let dd = d1 - d0
  a <- getAddr name
  bs <- mapM (`newAddrD` name) [1..dd]
  let b = last bs
  emitInstrs (zipWith IMov (a : bs) bs)
  --emitInstrs [IDelay dd (d1 - dd, d1) a b]
  insertVarInfo name $ VI True d1 b
  return b

advanceTo :: Bool -> Delay -> AName -> Emit Addr
advanceTo b d1 name = do
  (d0, a) <- getUnusedVarInfo name
  a' <- delayVar d0 d1 name
  insertVarInfo name $ VI b d1 a'
  return a'

newVar, updateDelay :: Delay -> AName -> Emit Addr
newVar d name = do
  a <- newAddr name
  insertVarInfo name $ VI False d a
  return a

updateDelay d name = do
  (_, a) <- getUnusedVarInfo name
  insertVarInfo name $ VI False d a
  return a

emitSingle :: (BName, ArgsPair) -> Emit Delay
emitSingle (bn, ap@(AP xs ys)) = do
  d <- getCurDelay
  mapM_ (advanceTo True d) ys
  emitSingle' bn ap

emitSingle' :: BName -> ArgsPair -> Emit Delay
emitSingle' BI (AP xs ys) | length xs == length ys =
  withOuts xs $ do
    xs' <- mapM getAddr xs
    ys' <- mapM getAddr ys
    emitInstrs $ zipWith IMov ys' xs'
    succ <$> getCurDelay

emitSingle' BI (AP _ _) = emitErr $ BIArityErr BI

emitSingle' BTI (AP xs@[a, b] [c, d, e]) =
  withOuts xs $ do
    (a', b', c', d', e') <- toAddr5 a b c d e
    emitInstrs [ICMov True a' b' c' d' e']
    succ <$> getCurDelay

emitSingle' BT (AP xs@[a, b, c] [d, e]) =
  withOuts xs $ do
    (a', b', c', d', e') <- toAddr5 a b c d e
    emitInstrs [ICMov False d' e' a' b' c']
    succ <$> getCurDelay

emitSingle' BTI (AP _ _) = emitErr $ BIArityErr BTI
emitSingle' BT (AP _ _) = emitErr $ BIArityErr BT

emitSingle' (BN instName name) ap =
  getBD name >>= emitSingleBD instName ap

emitSingle' (BNI instName name) ap =
  invBD <$> getBD name >>= emitSingleBD instName ap

emitSingleBD :: String -> ArgsPair -> BlockDef -> Emit Delay
emitSingleBD instName (AP outs ins) bd@BD { bdIn, bdOut } =
  withOuts outs $ do
    scope <- getScope
    EC { ecBDs } <- ask
    inAddrs <- mapM getAddr ins
    --error $ show ("bdIn:", bdIn, "inArgs:", zip ins inAddrs)
    let ctx' = EC { ecIns = M.fromList $ zip bdIn inAddrs
                  , ecOuts = bdOut
                  , ecScope = scope
                  , ecBDs }

    (outAddrs, dd) <- local (const ctx') $ emitBD instName bd

    forM_ (zip bdOut outs) $ \(bdOutVar, outVar) ->
      case M.lookup bdOutVar outAddrs of
        Nothing -> error $ show (outAddrs, bdOutVar, outVar)
        Just outAddr -> insertVarInfo outVar $ VI False (-1) outAddr

    return dd

getBD :: FName -> Emit BlockDef
getBD name = ask >>= \EC { ecBDs } ->
  case M.lookup name ecBDs of
    Nothing -> emitErr $ BDNotFound name
    Just bd -> return bd

withOuts :: [AName] -> Emit Delay -> Emit Delay
withOuts xs m = do
  d0 <- getCurDelay
  mapM_ (newVar $ -1000) xs
  d <- m
  s <- get
  mapM_ (updateDelay d) xs
  return d

toAddr5 :: AName -> AName -> AName -> AName -> AName
           -> Emit (Addr, Addr, Addr, Addr, Addr)
toAddr5 a b c d e = do
  a' <- getAddr a
  b' <- getAddr b
  c' <- getAddr c
  d' <- getAddr d
  e' <- getAddr e
  return (a', b', c', d', e')

