{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.VCGen ( invs
                               ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Maybe
import           Data.List
import qualified Data.HashSet             as S
import qualified Data.HashMap.Strict      as M

import           Verylog.Transform.TransitionRelation
import           Verylog.Transform.Utils as U
import           Verylog.Language.Types

import           Verylog.Solver.Common
import           Text.Printf

data PropertyOptions = PropertyOptions { checkTagEq :: Bool
                                       , checkValEq :: Bool
                                       }

defaultPropertyOptions :: PropertyOptions
defaultPropertyOptions = PropertyOptions { checkTagEq = True
                                         , checkValEq = False
                                         }

--------------------------------------------------------------------------------
invs :: [Id] -> [AlwaysBlock] -> [Inv]
--------------------------------------------------------------------------------
invs srcs as =
  concatMap (modular_inv srcs) as
  ++ non_interference_checks srcs as
  ++ concatMap (provedProperty defaultPropertyOptions) as

--------------------------------------------------------------------------------
modular_inv :: [Id] -> AlwaysBlock -> [Inv]
--------------------------------------------------------------------------------
modular_inv srcs a =
  [ initial_inv
  , tag_reset_inv
  , src_reset_inv
  , next_step_inv
  ] <*> [srcs] <*> [a']
  where
    a' = dbg (printf "\nalways block #%d:\n%s" (a^.aId) (show a)) a
    -- a' = trc (printf "\nalways block #%d:\n" (a^.aId)) a a
    -- a' = trc (printf "\nalways block #%d: " (a^.aId)) (length $ makeInvArgs fmt a) a

--------------------------------------------------------------------------------
initial_inv :: [Id] -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
initial_inv srcs a =
  Horn { hBody = Boolean True
       , hHead = Ands [ KV { kvId   = a ^. aId
                           , kvSubs = filterSubs a (sub1 ++ sub2)
                                      ++ [ (v, Boolean False) | v <- makeBothTags srcs]
                           }
                      ]
       , hId   = HornId i (InvInit i)
       }
  where
    i    = a ^. aId
    st   = a ^. aSt
    sub1 = [ (n_lvar sntz, rvar sntz)
           | sntz <- S.toList . S.fromList $
                     st ^. sanitize
           ]
    sub2 = [ (t, Boolean False)
           | t <- makeInvTags fmt a
           ]

--------------------------------------------------------------------------------
tag_reset_inv :: [Id] -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
tag_reset_inv srcs a =
  Horn { hBody =  prevKV a
       , hHead = KV { kvId   = i
                    , kvSubs = filterSubs a hsubs
                               ++ [ (v, Boolean True) | v <- makeBothTags srcs]
                    }
       , hId   = HornId i (InvReTag i)
       }
  where
    i      = a ^. aId
    hsubs  = [(t, Boolean False) | t <- makeBothTags $ (getRegisters a \\ srcs)]


--------------------------------------------------------------------------------
src_reset_inv :: [Id] -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
src_reset_inv srcs a =
  Horn { hBody =  prevKV a
       , hHead = KV { kvId   = i
                    , kvSubs = [ (v, Boolean False) | v <- makeBothTags srcs]
                    }
       , hId   = HornId i (InvSrcReset i)
       }
  where
    i      = a ^. aId

--------------------------------------------------------------------------------
next_step_inv :: [Id] -> AlwaysBlock -> Inv 
--------------------------------------------------------------------------------
next_step_inv srcs a =
  Horn { hBody = body
       , hHead = KV { kvId   = i
                    , kvSubs = filterSubs a subs
                    }
       , hId   = HornId i (InvNext i)
       }
  where
    i        = a ^. aId
    subs     = ul ++ ur
    (nl,ul)  = next fmt{leftVar=True}  a
    (nr,ur)  = next fmt{rightVar=True} a
    body     = Ands [ prevKV a
                    , sanGlobs (a^.aSt^.sanitizeGlob) subs
                    , taintEqs (a^.aSt^.taintEq) subs
                    , sourcesAreEqual srcs
                    , nl, nr
                    ]

-- wire input sources
sourcesAreEqual :: [Id] -> Expr
sourcesAreEqual srcs = Ands $ h <$> twoPairs srcs
  where
    h (x,y)  = let fl = fmt{taggedVar=True, leftVar=True}
                   fr = fmt{taggedVar=True, rightVar=True}
                   xl = makeVar fl x
                   xr = makeVar fr x
                   yl = makeVar fl y
                   yr = makeVar fr y
               in  Ands [ BinOp IFF xl yr
                        , BinOp IFF xr yl
                        , BinOp IFF xl xr
                        , BinOp IFF yl yr
                        ]


type Subs = [(Id,Expr)]

-- sanitize globs are always the same
sanGlobs        :: [Id] -> Subs -> Expr
sanGlobs vs subs = alwaysEqs conf vs subs
  where
    conf = AEC { isInitEq  = True
               , isPrimeEq = True
               , isValEq   = True
               , isTagEq   = True
               }

taintEqs        :: [Id] -> Subs -> Expr
taintEqs vs subs = alwaysEqs conf vs subs
  where
    conf = AEC { isInitEq  = True
               , isPrimeEq = True
               , isValEq   = False
               , isTagEq   = True
               }

data AlwaysEqConfig = AEC { isInitEq  :: Bool
                          , isPrimeEq :: Bool
                          , isValEq   :: Bool
                          , isTagEq   :: Bool
                          }

alwaysEqs :: AlwaysEqConfig -> [Id] -> Subs -> Expr
alwaysEqs (AEC{..}) vs subs = Ands (initEq ++ primeEq)
  where
    fmts :: [VarFormat]
    fmts = (if isValEq then [fmt]                 else []) ++
           (if isTagEq then [fmt{taggedVar=True}] else [])
             
    initEq :: [Expr]
    initEq  =
      if   isInitEq
      then [ let o = if taggedVar f then IFF else EQU
             in  BinOp o
                 (makeVar f{leftVar=True} v)
                 (makeVar f{rightVar=True} v)
           | v <- vs, f <- fmts
           ]
      else []

    primeEq :: [Expr]
    primeEq =
      if   isPrimeEq
      then [ BinOp o exprL exprR
           | v <- vs, (exprL, exprR, o) <- findLastIfExists v
           ]
      else []

    findLastIfExists :: Id -> [(Expr, Expr, BinOp)]
    findLastIfExists v =
      catMaybes
      [ let vl = makeVarName f{leftVar=True} v
            vr = makeVarName f{rightVar=True} v
            o  = if taggedVar f then IFF else EQU
        in case (lookup vl subs, lookup vr subs) of
             (Just el, Just er) -> Just (el, er, o)
             _                  -> Nothing
                                   
      | f <- fmts
      ]
  
  
--------------------------------------------------------------------------------
non_interference_checks :: [Id] -> [AlwaysBlock] -> [Inv]
--------------------------------------------------------------------------------
non_interference_checks srcs as = non_int_chk as [] []
  where
    hasCommon :: S.HashSet Id -> S.HashSet Id -> Bool
    hasCommon s1 s2 = not . null $ S.intersection s1 s2  

    non_int_chk :: [AlwaysBlock] -> [(RWSet,AlwaysBlock)] -> [Inv] -> [Inv]
    non_int_chk []      _checked cs = cs
    non_int_chk (a1:a1s) checked cs =
      let cs'                     = foldl' f cs checked
          rw1@(r1,w1)             = readWriteSet a1
          f cs_prev ((r2,w2), a2) =
            if   hasCommon w1 w2
            then (non_interference_inv srcs a1 a2) : (non_interference_inv srcs a2 a1) : cs_prev
            else let t12 = if   hasCommon w1 r2
                           then (non_interference_inv srcs a1 a2) : cs_prev
                           else cs_prev
                 in  if   hasCommon w2 r1
                     then (non_interference_inv srcs a2 a1) : t12
                     else t12
      in non_int_chk a1s ((rw1, a1):checked) cs'

type RWSet = (S.HashSet Id, S.HashSet Id)

readWriteSet :: AlwaysBlock -> RWSet
readWriteSet a = let (r,w) = evalState (comp (a^.aStmt) >> get) ([], [])
                     r'    = readVars r
                     w'    = writeVars w
                 in (r' `S.intersection` regs, w' `S.intersection` regs)
  where
    us = a^.aSt^.ufs

    regs = S.fromList $ getRegisters a

    -- readVars  v = S.fromList . filterRegs a $ M.lookupDefault [v] v us
    -- writeVars v = S.fromList . filterRegs a $ if M.member v us then [] else [v]

    readVars l = foldl' (\s v -> case M.lookup v us of
                                   Nothing     -> S.insert v s
                                   Just (_,vs) -> foldl' (flip S.insert) s vs) S.empty l
    writeVars = S.fromList
    
    comp :: Stmt -> State ([Id],[Id]) ()
    comp (Block ss)            = sequence_ (comp <$> ss)
    comp (BlockingAsgn{..})    = do _1 %= (:) rhs
                                    _2 %= (:) lhs
    comp (NonBlockingAsgn{..}) = do _1 %= (:) rhs
                                    _2 %= (:) lhs
    comp (IfStmt{..})          = do _1 %= (:) ifCond
                                    comp thenStmt
                                    comp elseStmt
    comp Skip                  = return ()

--------------------------------------------------------------------------------
non_interference_inv :: [Id] -> AlwaysBlock -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
-- when a1 takes a step, a2 still holds
non_interference_inv srcs a1 a2 =
  Horn { hBody = body
       , hHead = KV { kvId   = a2 ^. aId
                    , kvSubs = filterSubs a2 updates2
                    }
       , hId   = HornId (a2 ^. aId) (InvInter (a1 ^. aId))
       }
  where
    (nl1,ul1) = next fmt{leftVar=True}  a1
    (nr1,ur1) = next fmt{rightVar=True} a1
    updates1  = ul1 ++ ur1
    lukap v   = case lookup v updates1 of
                  Nothing -> throw $ PassError $ "cannot find " ++ v ++ " in updates1"
                  Just e  -> (v,e)
    updates2    = updates2_1 ++ updates2_2
    updates2_1  = concat [ [ (n_lvar v,  lvar v)  -- l' = l
                           , (n_rvar v,  rvar v)  -- r' = r
                           , (n_ltvar v, ltvar v) -- lt' = lt
                           , (n_rtvar v, rtvar v) -- rt' = rt
                           ]
                         -- variables not updated by a1 stay the same
                         --- | v <- (getRegisters a2) \\ (getRegisters a1) 
                         | v <- varName <$> ((a2 ^. aSt ^. ports) \\ (a1 ^. aSt ^. ports)) 
                         ]
    updates2_2 = [ lukap v
                 --- | p <- (getRegisters a2) `intersect` (getRegisters a1) 
                 | p <- varName <$> ((a2 ^. aSt ^. ports) `intersect` (a1 ^. aSt ^. ports)) 
                 , v <- primes p
                 ]
    
    merge f = S.toList $ (S.fromList (view f a1)) `S.union` (S.fromList (view f a2))
    body   = Ands [ prevKV a1
                  , prevKV a2
                  , sanGlobs (merge (aSt.sanitizeGlob)) (updates1++updates2)
                  , taintEqs (merge (aSt.taintEq)) (updates1++updates2)
                  , sourcesAreEqual srcs
                  , nl1
                  , nr1
                  ]

provedProperty :: PropertyOptions -> AlwaysBlock -> [Inv]
provedProperty (PropertyOptions{..}) a = 
  (if checkTagEq then tagEq else []) ++
  (if checkValEq then valEq else []) ++
  assertEqs
  where
    i     = a ^. aId
    tagEq = [ Horn { hHead = BinOp IFF (ltvar s) (rtvar s)
                   , hBody = KV { kvId   = i
                                , kvSubs = [ (n_rtvar s, rtvar s)
                                           , (n_ltvar s, ltvar s)
                                           ]
                                }
                   , hId   = HornId i (InvTagEq i)
                   }
            | s <- filterRegs a $ a^.aSt^.sinks
            ]
    valEq = [ Horn { hHead =  BinOp EQU (lvar s) (rvar s)
                   , hBody = Ands [ KV { kvId   = i
                                       , kvSubs = [ (n_lvar s, lvar s)
                                                  , (n_rvar s, rvar s)
                                                  ]
                                       }
                                  ]
                   , hId   = HornId i (InvOther "l_sink=r_sink")
                   }
            | s <- filterRegs a $ a^.aSt^.sinks
            ]
    assertEqs =
      [ Horn { hHead =  BinOp EQU (lvar s) (rvar s)
             , hBody = Ands [ KV { kvId   = i
                                 , kvSubs = [ (n_lvar s, lvar s)
                                            , (n_rvar s, rvar s)
                                            ]
                                 }
                            ]
             , hId   = HornId i (InvOther "left var = right var")
             }
      | s <- filterRegs a $ a^.aSt^.assertEq
      ]

-------------------------------------------------------------------------------- 
-- Helper functions
-------------------------------------------------------------------------------- 
lvar, rvar, ltvar, rtvar :: Id -> Expr
lvar  = makeVar fmt{leftVar=True}
rvar  = makeVar fmt{rightVar=True}
ltvar = makeVar fmt{taggedVar=True, leftVar=True}
rtvar = makeVar fmt{taggedVar=True, rightVar=True}

n_lvar, n_rvar, n_ltvar, n_rtvar :: Id -> Id
n_lvar   = makeVarName fmt{leftVar=True}
n_rvar   = makeVarName fmt{rightVar=True}
n_ltvar  = makeVarName fmt{taggedVar=True, leftVar=True}
n_rtvar  = makeVarName fmt{taggedVar=True, rightVar=True}

prevKV   :: AlwaysBlock -> Expr
prevKV a = KV { kvId   = a^.aId
              , kvSubs = []
              } 
primes :: Id -> [Id]
primes v = [ makeVarName f v
           | f <- [ f'{leftVar=True}
                  , f'{rightVar=True}
                  , f'{taggedVar=True, leftVar=True}
                  , f'{taggedVar=True, rightVar=True}
                  ]
           ]
  where
    -- f' = fmt{primedVar=True}
    f' = fmt

filterRegs :: AlwaysBlock -> [Id] -> [Id]
filterRegs a vs =
  foldl' (\l v -> if   (Register v) `elem` (a ^. aSt ^. ports)
                  then v:l
                  else l
         ) [] vs

filterSubs :: AlwaysBlock -> [(Id,Expr)] -> [(Id,Expr)]
filterSubs a = filter (\(v,_) -> v `S.member` kv_vars)
  where
    kv_vars :: S.HashSet Id
    kv_vars =
      (S.fromList (allArgs fmt{leftVar=True} (a^.aSt)))
      `S.union`
      (S.fromList (allArgs fmt{rightVar=True} (a^.aSt)))

