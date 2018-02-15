{-# LANGUAGE RecordWildCards #-}
module Verylog.Transform.Utils where

import           Control.Lens
import           Control.Monad.Reader
import qualified Data.HashSet             as S
import qualified Data.HashMap.Strict      as M
import           Control.Exception
import           Text.Printf

import           Verylog.Language.Types
import           Verylog.HSF.Types

isUF   :: Id -> Reader St Bool
isUF v = views ufs (M.member v)

data VarFormat = VarFormat { taggedVar :: Bool
                           , primedVar :: Bool
                           , leftVar   :: Bool
                           , rightVar  :: Bool
                           , atomVar   :: Bool
                           }

fmt = VarFormat { taggedVar = False
                , primedVar = False
                , leftVar   = False
                , rightVar  = False
                , atomVar   = False
                } 

makeVarName :: VarFormat -> Id -> HSFVar
makeVarName (VarFormat{..}) v = printf "%sV%s%s%s_%s" atom pos tag prime v
  where
    atom | atomVar   = "v"
         | otherwise = ""

    tag  | taggedVar = "T"
         | otherwise = ""

    prime | primedVar = "1"
          | otherwise = ""

    pos   | leftVar             = "L"
          | rightVar            = "R"
          | leftVar && rightVar = throw (PassError "Both left & right requested from makeVarName")
          | otherwise           = ""

allArgs        :: VarFormat -> St -> [Id]
allArgs fmt st = let rs = st^.registers.to S.toList 
                     ws = st^.wires.to     S.toList
                     us = st^.ufs.to       M.keys
                     vs = rs ++ ws
                     os = us ++ [done_atom]
                 in (makeVarName fmt                    <$> vs)
                    ++ (makeVarName fmt{taggedVar=True} <$> vs)
                    ++ (makeVarName fmt                 <$> os)


          
nextArgs        :: VarFormat -> St -> [Id]
nextArgs fmt st = allArgs fmt st ++ allArgs fmt{primedVar=True} st

invArgs        :: VarFormat -> St -> [Id]
invArgs fmt st = allArgs fmt{leftVar=True} st ++ allArgs fmt{rightVar=True} st
