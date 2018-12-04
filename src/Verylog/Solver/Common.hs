{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Verylog.Solver.Common where

import Control.Lens
import Verylog.Language.Types
import GHC.Generics
import Control.DeepSeq
import qualified Data.Text as T

import Language.Fixpoint.Types (Fixpoint(..), Loc(..), showFix, dummySpan)
import qualified Text.PrettyPrint.HughesPJ as PP

data BinOp = EQU | LE | GE | OR | AND | PLUS | IMPLIES | IFF
           deriving (Show, Eq, Generic, Ord)

data InvType = InvInit     Int
             | InvReTag    Int
             | InvSrcReset Int
             | InvNext     Int
             | InvTagEq    Int
             | InvWF       Int
             | InvInter    Int
             | InvOther    T.Text
            deriving (Generic, Eq, Ord)

instance Fixpoint InvType where
  toFix (InvInit n)     = PP.text "init of block"      PP.<+> PP.int n
  toFix (InvReTag n)    = PP.text "re-tag of block"    PP.<+> PP.int n
  toFix (InvSrcReset n) = PP.text "src-reset of block" PP.<+> PP.int n
  toFix (InvNext n)     = PP.text "next of block"      PP.<+> PP.int n
  toFix (InvTagEq n)    = PP.text "tag eq of block"    PP.<+> PP.int n
  toFix (InvWF n)       = PP.text "wf of block"        PP.<+> PP.int n
  toFix (InvInter n)    = PP.text "interference with"  PP.<+> PP.int n
  toFix (InvOther s)    = PP.text $ T.unpack s

instance Show InvType where
  show = showFix

data HornId = HornId Int InvType
            deriving (Generic, Show)

instance Loc HornId where
  srcSpan _ = dummySpan

instance Fixpoint HornId where
  toFix (HornId n t) = PP.parens (toFix t)
                       PP.<+> PP.text "always block id:"
                       PP.<+> PP.int n

instance NFData InvType
instance NFData HornId

data Inv = Horn { hBody :: Expr -- body of the horn clause
                , hHead :: Expr -- head of the horn clause, must be a kvar
                , hId   :: HornId
                }
           deriving (Show, Generic)

data Expr = BinOp     { bOp   :: BinOp
                      , expL  :: Expr
                      , expR  :: Expr
                      }
          | Ands      [Expr]
          | Ite       { cnd     :: Expr
                      , expThen :: Expr
                      , expElse :: Expr
                      }
          | KV        { kvId   :: Int
                      , kvSubs :: [(Id,Expr)]
                      }
          | Var       Id
          | Boolean   Bool
          | Number    Int
          | UFCheck   { ufArgs  :: [(Expr,Expr)]
                      , ufNames :: (Expr,Expr)
                      , ufFunc  :: Id
                      }
          deriving (Show, Eq, Generic)

instance NFData BinOp
instance NFData Expr
instance NFData Inv

nextPred :: T.Text
nextPred = "next"

invPred :: T.Text
invPred  = "inv"

makeInvPred   :: AlwaysBlock -> T.Text
makeInvPred a = makeInv (a^.aId)

makeInv :: Int -> T.Text
makeInv n = "inv" `T.append` (T.pack $ show n)

