{-# LANGUAGE RecordWildCards #-}

module Verylog.Solver.HSF.Types where

import Verylog.Language.Types
import Text.PrettyPrint

import Verylog.Solver.Common

data QueryNaming = QueryNaming { hsfId   :: Int
                               , hsfArgs :: [Id]
                               }

type HSFVar = String

printArgs as = hcat $ punctuate (comma <> space) (text <$> as)

instance PPrint QueryNaming where
  toDoc (QueryNaming{..}) = text "query_naming("
                            <> text invPred <> int hsfId
                            <> lparen <> printArgs hsfArgs <> text "))."

instance PPrint Inv where
  toDoc (Inv{..})  = text invPred <> int invId <> lparen <> printArgs invArgs <> text ") :-" 
                     $+$ text "("
                     $+$ nest 3 (toDoc invBody)
                     $+$ text ")."
  toDoc (Prop{..}) = toDoc propR <+> text ":-" 
                     $+$ nest 3 (toDoc propL) <> text "."
                            

instance PPrint Expr where
  toDoc (Boolean True)   = text "true"
  toDoc (Boolean False)  = text "false"
  toDoc (Number n)       = text $ show n
  toDoc (Var x)          = text x
  toDoc (Ands [])        = text "true"
  toDoc (Ands es)        = cat $ punctuate (comma <> space) (toDoc <$> es)
  toDoc (Structure f as) = text f <> lparen <> printArgs as <> rparen
  toDoc (Ite{..})        = text "ite("
                           <> toDoc  cnd <> comma
                           <+> toDoc expThen <> comma
                           <+> toDoc expElse
                           <> rparen
  toDoc (UnOp{..})       = case uOp of
                             NOT -> text "\\+" <> ptoDoc exp
  toDoc (BinOp{..})      = case bOp of
                             IMPLIES -> lparen
                                        <> ptoDoc expL
                                        <+> text "->"
                                        <+> ptoDoc expR
                                        <> rparen
                             EQU     -> toDoc expL <+> equals    <+> toDoc expR
                             LE      -> toDoc expL <+> text "=<" <+> toDoc expR
                             GE      -> toDoc expL <+> text ">=" <+> toDoc expR
                             PLUS    -> toDoc expL <+> text "+"  <+> toDoc expR
                             AND     -> cat [ toDoc expL <> comma
                                            , toDoc expR
                                            ]
                             OR      -> vcat [ lparen <> text "   " <> toDoc expL
                                             , semi   <> text "   " <> toDoc expR
                                             , rparen
                                             ]
  toDoc (UFCheck{..}) = toDoc d
    where
      d          = BinOp OR (UnOp NOT antecedent) consequent
      antecedent = Ands $ uncurry (BinOp EQU) <$> ufArgs
      consequent = uncurry (BinOp EQU) ufNames
      

ptoDoc :: PPrint a => a -> Doc
ptoDoc = parens . toDoc

instance Show Expr where
  show = pprint

instance Show Inv where
  show = pprint

instance Show QueryNaming where
  show = pprint
