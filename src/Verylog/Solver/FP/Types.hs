{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Verylog.Solver.FP.Types where

import           Control.Exception
import           Control.Monad.Reader
import           Control.Lens
import           Text.PrettyPrint
import           Text.Printf
import qualified Data.Set                   as S
import qualified Data.HashMap.Strict        as M

import           Verylog.Language.Types hiding (St, ufs)
import           Verylog.Solver.Common

type SMTVar = String

data FQBind = FQBind { bindId   :: Int
                     , bindName :: Id
                     , bindType :: String
                     , bindRef  :: String
                     }

data InvFun = InvFun { invFunName  :: Id
                     , invFunArity :: Int
                     }

data UFConst = UFConst { ufConstName  :: Id
                       , ufConstArity :: Int
                       }

data FPSt = FPSt { _constraints :: [Inv]
                 , _invs        :: [InvFun]
                 , _ufs         :: [UFConst]
                 , _binds       :: M.HashMap Id FQBind
                 }

makeLenses ''FPSt

type R    = Reader FPSt
type RDs  = Reader FPSt [Doc]
type Pr a = a -> R Doc

instance PPrint FPSt where
  toDoc st = runReader comp st
    where
      comp :: R Doc
      comp = do let qs = qualifiers
                bs <- printBinds
                cs <- makeConstraints
                wf <- makeWFConstraints
                return $ vcat $ punctuate nl $ qs : (vcat <$> [bs, cs, wf])

nl = text "\n"

qualifiers :: Doc
qualifiers = vcat $ text <$> [ "qualif Eq(x:int, y:int) : (x = y)"
                             ]
printBinds :: RDs
printBinds = do
  bs <- views binds M.elems
  mapM printBind bs

printBind :: Pr FQBind
printBind (FQBind{..}) = 
  return $
  text "bind" <+> int bindId <+> text bindName <+> colon <+>
  typeDef (text bindType) (text bindRef)

printExpr :: Pr Expr
printExpr (Boolean True)   = return $ text "true"
printExpr (Boolean False)  = return $ text "false"
printExpr (Number n)       = return $ text $ show n
printExpr (Var x)          = return $ text x
printExpr (Ands [])        = return $ text "true"
printExpr (Ands as)        = mapM printExpr as >>= return . brackets . sep . punctuate semi
printExpr (Ite{..})        = printExpr $ Ands [ BinOp IMPLIES cnd expThen
                                              , BinOp OR      cnd expElse
                                              ]
printExpr (Structure f as) = return $ text f <> args
  where
    mkSet (l,r) = text (printf "[%s := %s]" l r)
    args        = hcat $ mkSet <$> zip (fst <$> argVars' f as) as
printExpr (BinOp{..})      = do
  l <- printExpr expL
  r <- printExpr expR
  let pl = parens l
      pr = parens r
      op = case bOp of
             IMPLIES -> "==>"
             EQU     -> "="
             LE      -> "<="
             GE      -> ">="
             PLUS    -> "+"
             AND     -> "&&"
             OR      -> "||"
  return $ parens $ sep [pl, text op, pr]
printExpr (UFCheck{..}) = printExpr (Boolean True)

makeConstraints :: RDs
makeConstraints = view constraints >>= mapM helper . zip [1..]
  where
    helper                 :: Pr (Int, Inv)
    helper (n, (Inv{..}))  = mkC n invBody (Structure (makeInv invId) invArgs)
    helper (n, (Prop{..})) = mkC n propL   propR
  
    mkC :: Int -> Expr -> Expr -> R Doc
    mkC i expL expR = do
      ids <- getBindIds [expL, expR]
      l   <- printExpr expL
      r   <- printExpr expR
      
      let body = vcat [ text "env" <+> brackets (hsep $ punctuate semi (int <$> ids))
                      , text "lhs" <+> typeDef (text "int") l
                      , text "rhs" <+> typeDef (text "int") r
                      , text "id" <+> int i <+> text "tag []"
                      ]
      let res  = vcat [ text "constraint:"
                      , nest 2 body
                      , text " "
                      ]
      return res

makeWFConstraints :: RDs
makeWFConstraints = view invs >>= mapM helper
  where
    helper                  :: Pr InvFun
    helper inv@(InvFun{..}) = do
      let (arg1:args) = fst <$> argVars inv
      ids <- getBindIds (Var <$> args)

      let body = vcat [ text "env"  <+> brackets (hsep $ punctuate semi (int <$> ids))
                      , text "reft" <+> braces ( 
                          text arg1 <+> colon <+> text "int" <+>
                          text "|" <+> text "int"
                          )
                      ]
      return $ vcat [ text "wf:"
                    , nest 2 body
                    , text " "
                    ]

typeDef :: Doc -> Doc -> Doc
typeDef ty ref = 
    braces (text "v" <> colon <+> ty <+> text "|" <+> ref)

getBindIds :: [Expr] -> R [Int]
getBindIds es = mapM getBindId ids
  where
    ids   = S.toList idSet
    idSet = foldr (\e s -> s `S.union` getIds e ) S.empty es

    getBindId   :: Id -> R Int
    getBindId v = views binds (bindId . (M.lookupDefault (err v) v))

    err v = throw $ PassError $ printf "cannot find %s in binders" v

    helper []     = S.empty
    helper (e:es) = foldr (\e s -> getIds e `S.union` s) (getIds e) es

    getIds :: Expr -> S.Set Id
    getIds (BinOp{..})      = helper [expL, expR]
    getIds (Ands es)        = helper es
    getIds (Ite{..})        = helper [cnd, expThen, expElse]
    getIds (Structure f as) = S.fromList (as ++ args)
      where
        args = tail $ fst <$> argVars' f as
    getIds (Var v)          = S.singleton v
    getIds (UFCheck{..})    = 
      let (as1,as2) = unzip $ map (over both idFromExp) ufArgs
          (n1,n2)   = ufNames & both %~ idFromExp
      in S.fromList $ n1:n2:as1 ++ as2
    getIds (Number _)       = S.empty
    getIds (Boolean _)      = S.empty

idFromExp :: Expr -> Id
idFromExp (Var v) = v
idFromExp _       = throw $ PassError "given expr is not a variable"

instance Show FPSt where
  show = pprint

argVars :: InvFun -> [(Id,Int)]
argVars (InvFun{..}) = 
  let name n1 = printf "arg_%s_%d" invFunName n1
      ns      = [1..invFunArity]
  in zip (name <$> ns) ns

argVars' :: Id -> [Id] -> [(Id,Int)]
argVars' f as = argVars InvFun{ invFunName  = f
                              , invFunArity = length as
                              }
