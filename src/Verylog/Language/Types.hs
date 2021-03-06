{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Verylog.Language.Types where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict      as M
import qualified Data.HashSet             as S
import           Data.Typeable
import           Text.PrettyPrint hiding (sep)
import           Data.List
import qualified Data.Semigroup as SG
import           Data.Hashable
import qualified Data.Monoid as Mo
import           GHC.Generics hiding (to)
import           Control.DeepSeq

--------------------------------------------------------------------------------
-- IR for the formalism
--------------------------------------------------------------------------------

data AlwaysBlock = AB { _aEvent   :: ! Event
                      , _aStmt    :: ! Stmt
                      , _aId      :: ! Int
                      , _aSt      :: ! St
                      , _aLoc     :: ! (String, String) -- Module & instance name
                      }
                   deriving (Generic)

--------------------------------------------------------------------------------
-- Intermediary IR after parsing
--------------------------------------------------------------------------------

type Id = String

data Port = Input  { portName :: ! String }
          | Output { portName :: ! String }
          deriving (Eq, Generic)

instance Show Port where
  show (Input  i) = "input("  ++ i ++ ")"
  show (Output o) = "output(" ++ o ++ ")"

instance Hashable Port where
  hashWithSalt n (Input s)  = hashWithSalt n ("input", s)
  hashWithSalt n (Output s) = hashWithSalt n ("output", s)

data Var = Register { varName :: ! String }
         | Wire     { varName :: ! String }
         deriving (Eq, Generic)

instance Show Var where
  show (Register r) = "register(" ++ r ++ ")"
  show (Wire     w) = "wire("     ++ w ++ ")"


instance Hashable Var where
  hashWithSalt n (Register s) = hashWithSalt n ("register", s)
  hashWithSalt n (Wire s)     = hashWithSalt n ("wire", s)

data IR = Always     { event      :: ! Event
                     , alwaysStmt :: ! Stmt
                     , alwaysLoc  :: ! (String, String) -- Module & instance name
                     }
        | ModuleInst { modInstName :: ! String
                     , modParams   :: ! [Port] -- formal parameters
                     , modInstSt   :: ! St
                     }
        deriving (Generic)

data Event = Star
           | PosEdge { eventVar :: Id }
           | NegEdge { eventVar :: Id }
           | Assign
           deriving (Eq, Generic)

data Stmt = Block           { blockStmts :: ! [Stmt] }
          | BlockingAsgn    { lhs        :: ! Id
                            , rhs        :: ! Id
                            }
          | NonBlockingAsgn { lhs        :: ! Id
                            , rhs        :: ! Id
                            }
          | IfStmt          { ifCond     :: ! Id
                            , thenStmt   :: ! Stmt
                            , elseStmt   :: ! Stmt
                            }
          | Skip
          deriving (Generic)

type UFMap = M.HashMap Id (Id, [Id])

data St = St { _ports        :: ! [Var]
             , _ufs          :: ! UFMap
             , _sources      :: ! [Id]
             , _sinks        :: ! [Id]
             , _taintEq      :: ! [Id]
             , _assertEq     :: ! [Id]
             , _sanitize     :: ! [Id]
             , _sanitizeGlob :: ! [Id]
             , _irs          :: ! [IR]
             }
          deriving (Generic)

emptySt :: St
emptySt = St { _ports        = []
             , _ufs          = M.empty
             , _sources      = []
             , _sinks        = []
             , _assertEq     = []
             , _taintEq      = []
             , _sanitize     = []
             , _sanitizeGlob = []
             , _irs          = []
             }

makeLenses ''St 
makeLenses ''AlwaysBlock

done_atom :: Id
done_atom = "done"

runIRs :: (IR -> State St a) -> State St [a]
runIRs f = use irs >>= sequence . (map f)

runIRs_ :: (IR -> State St a) -> State St ()
runIRs_ f = use irs >>= sequence_ . (map f)

readIRs :: St -> (IR -> Reader St a) -> [a]
readIRs st f = st^.irs.to (map (r . f))
  where
    r m = runReader m st

data PassError = PassError !String
               | CycleError { cycleStr      :: !String
                            , cycleErrorStr :: !String
                            }
               deriving (Show, Typeable)

instance Exception PassError
  

-- -----------------------------------------------------------------------------  
-- Pretty printing
-- -----------------------------------------------------------------------------  

class PPrint a where
  toDoc :: a -> Doc

  pprint :: a -> String
  pprint = (renderStyle style{ lineLength     = 150
                             , ribbonsPerLine = 1.2
                             }) . toDoc

instance PPrint IR where
  toDoc (Always{..})      = text "always(" <> vcat [toDoc event <> comma, toDoc alwaysStmt] <> text ")."
  toDoc (ModuleInst{..})  = text "module" <> vcat [ lparen <+> text modInstName
                                                  , comma  <+> pl ((text . portName) <$> modParams)
                                                  , comma  <+> toDoc modInstSt
                                                  , rparen
                                                  ] <> text "."
    where
      pl = brackets . hsep . (punctuate (text ", "))
                            
  
instance PPrint Stmt where
  toDoc (Block [])     = brackets empty
  toDoc (Block (s:ss)) = vcat $ (lbrack <+> toDoc s) : (((comma <+>) . toDoc) <$> ss) ++ [rbrack]
  toDoc (BlockingAsgn{..}) = 
    text "b_asn(" <> text lhs <> comma <+> text rhs <> rparen
  toDoc (NonBlockingAsgn{..}) = 
    text "nb_asn(" <> text lhs <> comma <+> text rhs <> rparen
  toDoc (IfStmt{..}) = text "ite" <> vcat [ lparen <+> text ifCond
                                          , comma  <+> toDoc thenStmt
                                          , comma  <+> toDoc elseStmt
                                          , rparen
                                          ]
  toDoc Skip = text "skip"

instance PPrint Event where
  toDoc Star          = text "event1(star)"
  toDoc Assign        = text "assign"
  toDoc (PosEdge clk) = text "event2(posedge," <> text clk <> rparen
  toDoc (NegEdge clk) = text "event2(negedge," <> text clk <> rparen

instance PPrint a => PPrint [a] where
  toDoc as = brackets $ hsep $ punctuate comma (map toDoc as)

instance PPrint Var where
  toDoc (Register r) = text "register" <> parens (text r)
  toDoc (Wire w)     = text "wire" <> parens (text w)

instance PPrint St where
  toDoc st = vcat $ stDoc : space : st^.irs.to (map toDoc)
    where
      stDoc = text "St" <+>
              vcat [ lbrace <+> text "ports" <+> equals <+> st^.ports.to     toDoc
                   , comma  <+> text "ufs  " <+> equals <+> st^.ufs.to       printMap
                   , comma  <+> text "srcs " <+> equals <+> st^.sources.to   printList
                   , comma  <+> text "sinks" <+> equals <+> st^.sinks.to     printList
                   , comma  <+> text "sntz " <+> equals <+> st^.sanitize.to  printList
                   , rbrace
                   ]

instance PPrint AlwaysBlock where
  toDoc a = text "always(" <> vcat [ comment "id       " <+> int (a^.aId)                     <> comma
                                   , comment "mod name " <+> text (a^.aLoc^._1)               <> comma
                                   , comment "inst name" <+> text (a^.aLoc^._2)               <> comma
                                   , comment "ports    " <+> toDoc (a^.aSt^.ports)            <> comma
                                   , comment "ufs      " <+> printMap  (a^.aSt^.ufs)          <> comma
                                   , comment "sources  " <+> printList (a^.aSt^.sources)      <> comma
                                   , comment "sinks    " <+> printList (a^.aSt^.sinks)        <> comma
                                   , comment "sanitize " <+> printList (a^.aSt^.sanitize)     <> comma
                                   , comment "san. glob" <+> printList (a^.aSt^.sanitizeGlob) <> comma
                                   , comment "taint eq." <+> printList (a^.aSt^.taintEq)      <> comma
                                   , comment "asrt. eq"  <+> printList (a^.aSt^.assertEq)     <> comma
                                   , toDoc (a^.aEvent)                                        <> comma
                                   , toDoc (a^.aStmt)
                                   ] <> text ")."
    where
      comment t = text "/*" <+> text t <+> text "*/"

printList :: [String] -> Doc
printList = brackets . text . (intercalate ", ")

printMap :: UFMap -> Doc
printMap = brackets
           . text
           . (intercalate ", ")
           . (map mapKV)
           . M.toList
  where
    mapKV (k,(f, l)) = "(" ++ k ++ ", " ++ f ++ ", [" ++ (intercalate ", " l) ++ "])"

instance Show IR where
  show = pprint
instance Show Stmt where
  show = pprint
instance Show Event where
  show = pprint
instance Show St where
  show = pprint
instance Show AlwaysBlock where
  show = pprint

instance NFData Event
instance NFData Stmt
instance NFData Var
instance NFData Port
instance NFData IR
instance NFData St
instance NFData AlwaysBlock

instance SG.Semigroup St where
  m1 <> m2 = 
    St { _ports        = jn_list ports
       , _ufs          = (m1 ^. ufs) Mo.<> (m2 ^. ufs)
       , _sources      = jn_list sources
       , _sinks        = jn_list sinks
       , _taintEq      = jn_list taintEq
       , _assertEq     = jn_list assertEq
       , _sanitize     = jn_list sanitize
       , _sanitizeGlob = jn_list sanitizeGlob
       , _irs          = (m1 ^. irs) Mo.<> (m2 ^. irs)
       }
    where
      jn_list fld =
        S.toList $
        S.fromList (m1 ^. fld) Mo.<> 
        S.fromList (m2 ^. fld)

instance Mo.Monoid St where
  mempty  = emptySt
  mappend = (SG.<>)

getRegisters :: AlwaysBlock -> [Id]
getRegisters a =
  map varName $ filter isRegister (a ^. aSt ^. ports)

isRegister :: Var -> Bool
isRegister (Register _) = True
isRegister (Wire _)     = False

class FoldVariables a where
  foldVariables :: a -> [Id]

instance FoldVariables Stmt where
  foldVariables (Block ss)            = concatMap foldVariables ss
  foldVariables (BlockingAsgn l r)    = [l, r]
  foldVariables (NonBlockingAsgn l r) = [l, r]
  foldVariables (IfStmt c t e)        = [c] ++ concatMap foldVariables [t,e]
  foldVariables Skip                  = []

instance FoldVariables IR where
  foldVariables (Always _ s _) = foldVariables s
  foldVariables _              = throw (PassError "foldVariables called on non-always block")

isClk :: Event -> Bool
isClk Assign      = False
isClk Star        = False
isClk (NegEdge _) = True
isClk (PosEdge _) = True

instance Hashable AlwaysBlock where
  hashWithSalt n (AB{..}) = hashWithSalt n _aId

instance Eq AlwaysBlock where
  a1 == a2 = (a1 ^. aId) == (a2 ^. aId)
