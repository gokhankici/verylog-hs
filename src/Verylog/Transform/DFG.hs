{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.DFG
  ( wireTaints
  , assignmentMap
  , stmt2Assignments
  
  , AM
  , hasCycle
  , readSets
  , writeSets
  , pathsToNonAssigns
  , getLhss
  ) where

import           Control.Lens            hiding (mapping)
import qualified Data.HashSet            as HS
import qualified Data.HashMap.Strict     as HM
import qualified Data.IntMap.Strict      as IM
import qualified Data.IntSet             as IS
import           Text.Printf
import           Verylog.Language.Types
import           Data.Monoid 

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query hiding (trc)

--------------------------------------------------------------------------------
wireTaints :: AlwaysBlock -> [Id] -> [Id]
--------------------------------------------------------------------------------
-- takes an always block and a list of wires/registers, and returns the registers that
-- should be tainted by the analysis at the initial state
wireTaints a srcs =  rs ++ (HS.toList $ worklist a (assignmentMap a) ws)
  where
    (rs,ws) = let f p (rss,wss) =
                    let l = filter (\p' -> varName p' == p) prts
                    in case l of
                         []             -> error $ printf "cannot find %s in ports" p
                         (Register _):_ -> (p:rss, wss)
                         (Wire _):_     -> (rss, p:wss)
              in  foldr f ([],[]) srcs
    prts    = a ^. aSt ^. ports

assignmentMap :: AlwaysBlock -> M
assignmentMap a = stmt2Assignments (a ^. aStmt) (a ^. aSt ^. ufs)

worklist :: AlwaysBlock -> M -> [Id] -> S
worklist a assignments wl = h (wl, HS.empty, HS.empty)
  where
    h :: ([Id], S, S) -> S
    h (wrklst, donelst, reglst) =
      case wrklst of
        []        -> reglst
        p:wrklst_ ->
          let donelst' = HS.insert p donelst
              reglst'  = if   HS.member p regs
                         then HS.insert p reglst
                         else reglst
              rhss     = case HM.lookup p assignments of
                           Nothing -> []
                           Just l  -> HS.toList l
              wrklst'  = foldr (\rhs wl' -> if   HS.member rhs donelst
                                            then wl'
                                            else rhs:wl') wrklst_ rhss
          in h (wrklst', donelst', reglst')

    regs = foldr (\p s -> case p of
                            Register r -> HS.insert r s
                            _          -> s)
           HS.empty (a ^. aSt ^. ports)

stmt2Assignments :: Stmt -> HM.HashMap Id [Id] -> M
stmt2Assignments s unintFuncs = h [] s
  where
    h :: [Id] -> Stmt -> M
    h _ Skip                  = HM.empty
    h l (BlockingAsgn{..})    = h2 (l2ls rhs ++ l) lhs HM.empty 
    h l (NonBlockingAsgn{..}) = h2 (l2ls rhs ++ l) lhs HM.empty 
    h l (IfStmt{..})          = HM.unionWith HS.union
                                (h (l2ls ifCond ++ l) thenStmt)
                                (h (l2ls ifCond ++ l) elseStmt)
    h l (Block{..})           = foldr (HM.unionWith HS.union) HM.empty (h l <$> blockStmts)

    h2 :: [Id] -> Id -> M -> M
    h2 ls r m = foldr (\l m' -> HM.alter (\ml -> case ml of
                                             Nothing -> Just $ HS.singleton r
                                             Just rs -> Just $ HS.insert r rs
                                         ) l m') m ls

    l2ls   :: Id -> [Id]
    l2ls l = case HM.lookup l unintFuncs of
               Nothing -> [l]
               Just ls -> ls


type S = HS.HashSet Id
type M = HM.HashMap Id S

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type G  = Gr Bool ()
type AM = IM.IntMap AlwaysBlock
type M2 = HM.HashMap Id IS.IntSet

hasCycle :: Gr a b -> Bool
hasCycle g = any ((>= 2) . length) (scc g)

readSets :: [AlwaysBlock] -> IM.IntMap S
readSets as = IM.fromList $ (\a -> (a ^. aId, getRhss (a ^. aSt ^. ufs) (a ^. aStmt))) <$> as

writeSets :: [AlwaysBlock] -> IM.IntMap S
writeSets as = IM.fromList $ (\a -> (a ^. aId, getLhss (a ^. aStmt))) <$> as

pathsToNonAssigns :: AM -> IM.IntMap S -> IM.IntMap S -> [[Node]]
pathsToNonAssigns _gr = undefined
  -- where
  --   foo           = [ parentG r | r <- roots ] 
  --   rootG         = gfiltermap (\c -> if suc' c == [] then Just c else Nothing) gr
  --   roots         = fst <$> labNodes rootG
  --   parentG r     = subgraph (rdfs [r] gr) gr


makeGraphFromRWSet :: AM -> IM.IntMap S -> IM.IntMap S -> G
makeGraphFromRWSet abMap rs ws = mkGraph allNs es
  where
    allNs = 
      fmap (\n -> (n, (abMap IM.! n) ^. aEvent /= Assign)) $
      IS.toList $
      IM.keysSet rs `IS.union` IM.keysSet ws

    es :: [(Int, Int, ())]
    es =
      IM.foldlWithKey'
      (\l n s ->
          -- ns : all blocks that update the sensitivity list of block# n
          let ns = IS.toList $ foldMap (\v -> HM.lookupDefault IS.empty v sensitizers) s
          -- (n1, n2) means n1's block is ***after*** after n2 executes
          in ((\n' -> (n,n',())) <$> ns) ++ l
      )
      []
      rs

    -- v: variable ==> {n:block k# | n updates v}
    sensitizers :: M2
    sensitizers =
      let f v Nothing  = Just $ IS.singleton v
          f v (Just s) = Just $ IS.insert v s
      in IM.foldlWithKey'
         (\m n s -> HS.foldl' (\m' v -> HM.alter (f n) v m') m s)
         HM.empty
         ws

getLhss :: Stmt -> S
getLhss s = h s
  where
    h Skip                  = HS.empty
    h (BlockingAsgn{..})    = HS.singleton lhs
    h (NonBlockingAsgn{..}) = HS.singleton lhs
    h (IfStmt{..})          = foldMap h [thenStmt, elseStmt]
    h (Block{..})           = foldMap h blockStmts

getRhss :: HM.HashMap Id [Id] -> Stmt -> S
getRhss us s = h s
  where
    h Skip                  = HS.empty
    h (BlockingAsgn{..})    = lukap rhs
    h (NonBlockingAsgn{..}) = lukap rhs
    h (IfStmt{..})          = lukap ifCond <> foldMap h [thenStmt, elseStmt]
    h (Block{..})           = foldMap h blockStmts

    lukap :: Id -> S
    lukap v = case HM.lookup v us of
                Nothing -> HS.singleton v
                Just vs -> HS.fromList vs
