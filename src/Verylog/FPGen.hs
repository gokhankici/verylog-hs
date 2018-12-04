module Verylog.FPGen ( pipeline
                     , pipeline'
                     ) where

import Control.Arrow
import qualified Data.Text as T

import Verylog.Language.Parser
import Verylog.Language.Types
import Verylog.Transform.Modularize
import Verylog.Transform.Merge
import Verylog.Transform.SanityCheck
import Verylog.Transform.FPVCGen
import Verylog.Solver.FP.Types

--------------------------------------------------------------------------------
pipeline :: FilePath -> T.Text -> FPSt
--------------------------------------------------------------------------------
pipeline f = common f >>> toFpSt

  
--------------------------------------------------------------------------------
pipeline' :: FilePath -> T.Text -> [AlwaysBlock]
--------------------------------------------------------------------------------
pipeline' f = common f >>> arr fst

common :: FilePath -> T.Text -> ([AlwaysBlock], Annots)
common f = parse f
           >>> first flatten
           -- >>> first sanityCheck
           >>> merge
