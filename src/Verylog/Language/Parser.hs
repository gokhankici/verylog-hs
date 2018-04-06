{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Language.Parser ( parse
                               , renderError
                               , IRParseError (..)
                               ) where

import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.State.Lazy
import           Data.Char (isLetter, isDigit)
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as S
import qualified Data.List                  as Li
import qualified Data.List.NonEmpty         as NE
import           Data.Typeable
import           Text.Megaparsec            as MP hiding (parse, State(..))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf

import           Verylog.Language.Types
import           Verylog.Language.Utils
-- import           Verylog.Transform.Utils

-----------------------------------------------------------------------------------
-- | Verylog IR
-----------------------------------------------------------------------------------

data ParsePort = PRegister { parsePortName :: String }
               | PWire     { parsePortName :: String }
               deriving (Show)

data ParseBehavior = PAlways ParseEvent ParseStmt
                   deriving (Show)

data ParseEvent = PStar
                | PPosEdge String
                | PNegEdge String
                deriving (Show)

data ParseUF = PUF String [String]
               deriving (Show)

data ParseGate = PContAsgn String String
               | PModuleInst { pmInstName      :: String            -- name of the module
                             , pmInstPortNames :: [String]          -- port list (i.e. formal parameters)
                             , pmInstArgs      :: [String]          -- instantiations (i.e. actual parameters)
                             , pmInstPorts     :: [ParsePort]       -- wires or registers used
                             , pmInstGates     :: [ParseGate]       -- assign or module instantiations
                             , pmInstBehaviors :: [ParseBehavior]   -- always blocks
                             , pmInstUFs       :: [ParseUF]         -- uninterpreted functions
                             }
               deriving (Show)
                         
data ParseIR = TopModule { mPortNames :: [String]          -- port list (i.e. formal parameters)
                         , mPorts     :: [ParsePort]       -- wires os registers used
                         , mGates     :: [ParseGate]       -- assign or module instantiations
                         , mBehaviors :: [ParseBehavior]   -- always blocks
                         , mUFs       :: [ParseUF]         -- uninterpreted functions
                         }
             | PSource   String
             | PSink     String
             | PSanitize String
             deriving (Show)
             
data ParseStmt = PBlock           [ParseStmt]
               | PBlockingAsgn    String
                                  String
               | PNonBlockingAsgn String
                                  String
               | PIfStmt          String
                                  ParseStmt
                                  ParseStmt
               | PSkip
               deriving (Show)

data ParseSt = ParseSt { _parseSources :: S.HashSet Id
                       , _parseSinks   :: S.HashSet Id
                       , _parsePorts   :: S.HashSet Id
                       , _parseSanitize :: S.HashSet Id
                       , _st           :: St
                       }

emptyParseSt = ParseSt { _parseSources = S.empty
                       , _parseSinks   = S.empty
                       , _parsePorts   = S.empty
                       , _parseSanitize = S.empty
                       , _st           = emptySt
                       }

makeLenses ''ParseSt

type Parser = Parsec SourcePos String

-- --------------------------------------------------------------------------------
parse :: FilePath -> String -> St
-- --------------------------------------------------------------------------------
parse f = parseWith parseIR f >>> makeState

parseWith  :: Parser a -> FilePath -> String -> a
parseWith p f s = case runParser (whole p) f s of
                    Left err -> throw (IRParseError (parseErrorPretty err) (NE.head . errorPos $ err))
                    Right e  -> e

--------------------------------------------------------------------------------
-- | Top-Level Expression Parser
--------------------------------------------------------------------------------

parsePort :: Parser ParsePort
parsePort = rWord "register" *> parens (PRegister <$> identifier)
            <|> rWord "wire" *> parens (PWire     <$> identifier)

parseBehavior :: Parser ParseBehavior
parseBehavior = rWord "always" *> parens (PAlways <$> parseEvent <*> (comma *> parseStmt))

parseEvent :: Parser ParseEvent
parseEvent = rWord "event1(star)" *> return PStar
             <|> rWord "event2(posedge" *> comma *> (PPosEdge <$> identifier) <* rWord ")"
             <|> rWord "event2(negedge" *> comma *> (PNegEdge <$> identifier) <* rWord ")"

parseUF :: Parser ParseUF
parseUF = rWord "link" *> parens (PUF <$> identifier <*> (comma *> list identifier))

parseGate :: Parser ParseGate
parseGate = rWord "asn" *> parens (PContAsgn <$> identifier <*> (comma *> identifier))
            <|> parseModuleInst

parseModuleInst :: Parser ParseGate
parseModuleInst = rWord "module"
                  *> parens (PModuleInst
                              <$> identifier
                              <*> (comma *> list identifier)
                              <*> (comma *> list identifier)
                              <*> (comma *> list parsePort)
                              <*> (comma *> list parseGate)
                              <*> (comma *> list parseBehavior)
                              <*> (comma *> list parseUF))

parseTopModule :: Parser ParseIR
parseTopModule = spaceConsumer
                 *> rWord "topmodule"
                 *> parens (TopModule
                             <$> list identifier
                             <*> (comma *> list parsePort)
                             <*> (comma *> list parseGate)
                             <*> (comma *> list parseBehavior)
                             <*> (comma *> list parseUF))
                 <* char '.' <* spaceConsumer

parseTaint :: Parser ParseIR  
parseTaint = spaceConsumer
             *> ( rWord "taint_source" *> parens (PSource <$> taintId)
                  <|> rWord "taint_sink" *> parens (PSink <$> taintId)
                  <|> rWord "sanitize" *> parens (PSanitize <$> taintId)
                )
             <* char '.' <* spaceConsumer
  where
    taintId = identifier -- ("v_" ++) <$> identifier

parseStmt :: Parser ParseStmt  
parseStmt = rWord "block"      *> parens (PBlock           <$> list parseStmt)
            <|> rWord "b_asn"  *> parens (PBlockingAsgn    <$> identifier <*> (comma *> identifier))
            <|> rWord "nb_asn" *> parens (PNonBlockingAsgn <$> identifier <*> (comma *> identifier))
            <|> rWord "ite"    *> parens (PIfStmt          <$> identifier <*> (comma *> parseStmt) <*> (comma *> parseStmt))
            <|> rWord "skip"   *> return PSkip

parseIR :: Parser [ParseIR]
parseIR = (:) <$> parseTopModule <*> many parseTaint

--------------------------------------------------------------------------------
-- | Tokenisers and Whitespace
--------------------------------------------------------------------------------

-- | Top-level parsers (should consume all input)
whole :: Parser a -> Parser a
whole p = spaceConsumer *> p <* eof

spaceConsumer :: Parser ()
spaceConsumer = (L.space (void spaceChar) lineCmnt blockCmnt) -- *> (L.space (void spaceChar) prologDecl blockCmnt)
  where blockCmnt    = L.skipBlockComment "/*" "*/"
        lineCmnt     = L.skipLineComment "%"
        -- prologDecl   = L.skipLineComment ":-"

-- | `symbol s` parses just the string s (and trailing whitespace)
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

comma :: Parser String
comma = symbol ","

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = betweenS "(" ")"

list :: Parser a -> Parser [a]
list p = betweenS "[" "]" lp
  where
    lp = (:) <$> p <*> many (comma *> p)
         <|> return []

betweenS :: String -> String -> Parser a -> Parser a
betweenS l r = between (symbol l) (symbol r)

-- | `lexeme p` consume whitespace after running p
lexeme :: Parser a -> Parser a
lexeme p = L.lexeme spaceConsumer p

-- | `rWord`
rWord   :: String -> Parser String
rWord w = string w <* notFollowedBy alphaNumChar <* spaceConsumer

-- | list of reserved words
keywords :: [String]
keywords =
  [ "register", "wire", "always", "link", "asn", "taint_source", "taint_sink"
  , "block", "b_asn", "nb_asn", "ite", "skip", "module", "topmodule"
  ]

-- | `identifier` parses identifiers: lower-case alphabets followed by alphas or digits
identifier :: Parser String
identifier = lexeme (p >>= check)
  where
    p            = (:) <$> letterChar <*> many nonFirstChar
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')
    check x      = if x `elem` keywords
                   then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                   else return x

--------------------------------------------------------------------------------
-- | Printing Error Messages
--------------------------------------------------------------------------------

readFilePos    :: SourcePos -> IO String
readFilePos pos = getPos pos <$> readFile (MP.sourceName pos)

getPos :: SourcePos -> String -> String
getPos pos = getSpanSingle (unPos $ sourceLine pos) (unPos $ sourceColumn pos)

getSpanSingle :: Int -> Int -> String -> String
getSpanSingle l c
  = highlight l c 
  . safeHead ""
  . getRange l l
  . lines

highlight :: Int -> Int -> String -> String
highlight l c s = unlines
  [ cursorLine l s
  , replicate (12 + c) ' ' ++ "^"
  ]

cursorLine :: Int -> String -> String
cursorLine l s = printf "%s|  %s" (lineString l) s

lineString :: Int -> String
lineString n = replicate (10 - nD) ' ' ++ nS
  where
    nS       = show n
    nD       = Li.length nS

renderError :: IRParseError -> IO String
renderError e = do
  let pos = ePos e
      msg = eMsg e
  snippet <- readFilePos pos
  return $ printf "%s%s" snippet msg

instance ShowErrorComponent SourcePos where
  showErrorComponent pos = "parse error in file " ++ (MP.sourceName pos)

data IRParseError = IRParseError
  { eMsg :: !String
  , ePos :: !MP.SourcePos
  }
  deriving (Show, Typeable)

instance Exception IRParseError

-----------------------------------------------------------------------------------
-- | ParseIR -> St
-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
makeState :: [ParseIR] -> St
-----------------------------------------------------------------------------------
makeState (TopModule{..}:taints) = evalState comp emptyParseSt
  where
    comp = do sequence_ $ collectTaint <$> taints -- collect taint information
              st .= makeIntermediaryIR mPorts mGates mBehaviors mUFs -- create intermediary IR from parse IR
  
              -- update IR state's taint info from parse IR 
              st . sources  <~ uses parseSources  S.toList
              st . sinks    <~ uses parseSinks    S.toList
              st . sanitize <~ uses parseSanitize S.toList

              -- and copy it to the instantiated modules
              st %= updateTaintInfo 

              -- make sure we have at least one source and a sink
              let f = (== 0) . length
              noTaint <- liftM2 (||) (uses (st.sinks) f) (uses (st.sources) f)
              if noTaint
                then throw (PassError "Source or sink taint information is missing")
                else use st

makeState _ = throw (PassError "First ir is not a toplevel module !")

-----------------------------------------------------------------------------------
collectTaint :: ParseIR -> State ParseSt ()
-----------------------------------------------------------------------------------
collectTaint (PSource s)     = parseSources  %= S.insert s
collectTaint (PSink s)       = parseSinks    %= S.insert s
collectTaint (PSanitize s)   = parseSanitize %= S.insert s
collectTaint (TopModule{..}) = return ()
    
-----------------------------------------------------------------------------------
makeIntermediaryIR :: [ParsePort] -> [ParseGate] -> [ParseBehavior] -> [ParseUF] -> St
-----------------------------------------------------------------------------------
makeIntermediaryIR prts gates bhvs us = evalState comp emptyParseSt
  where
    comp = do sequence_ (collectPort <$> prts)
              sequence_ (collectGate <$> reverse gates)
              sequence_ (collectBhv  <$> reverse bhvs)
              sequence_ (collectUF   <$> reverse us)
              st . ports <~ uses parsePorts S.toList
              st . ufs   %= flattenUFs
              use st

    ------------------------------------------------------
    flattenUFs   :: M.HashMap Id [Id] -> M.HashMap Id [Id]
    ------------------------------------------------------
    -- make is so that arguments to the uninterpreted functions
    -- cannot be other uninterpreted functions
    -- i.e. they can be only registers or wires
    flattenUFs m = let varDeps v = case M.lookup v m of
                                     Nothing -> [v]
                                     Just as -> concatMap varDeps as
                   in M.mapWithKey (\k _ -> varDeps k) m

-----------------------------------------------------------------------------------
collectPort :: ParsePort -> State ParseSt ()
-----------------------------------------------------------------------------------
collectPort p = parsePorts %= S.insert (parsePortName p)

-----------------------------------------------------------------------------------
collectGate :: ParseGate -> State ParseSt ()
-----------------------------------------------------------------------------------
collectGate (PContAsgn l r)   = st . irs %= (:) (Always Star (BlockingAsgn l r))
collectGate (PModuleInst{..}) =
  st . irs %= (:) ModuleInst{ modInstName = pmInstName
                            , modInstArgs = zip pmInstPortNames pmInstArgs
                            , modInstSt   = st'
                            }
  where
    st' = makeIntermediaryIR pmInstPorts pmInstGates pmInstBehaviors pmInstUFs

-----------------------------------------------------------------------------------
collectBhv :: ParseBehavior -> State ParseSt ()
-----------------------------------------------------------------------------------
collectBhv (PAlways ev stmt) = st . irs %= (:) (Always (makeEvent ev) (makeStmt stmt))

-----------------------------------------------------------------------------------
makeEvent :: ParseEvent -> Event
-----------------------------------------------------------------------------------
makeEvent PStar          = Star
makeEvent (PPosEdge clk) = PosEdge clk
makeEvent (PNegEdge clk) = NegEdge clk

-----------------------------------------------------------------------------------
makeStmt :: ParseStmt -> Stmt
-----------------------------------------------------------------------------------
makeStmt (PBlock ss)            = Block (makeStmt <$> ss)
makeStmt (PBlockingAsgn l r)    = BlockingAsgn l r
makeStmt (PNonBlockingAsgn l r) = NonBlockingAsgn l r
makeStmt (PIfStmt cond th el)   = IfStmt cond (makeStmt th) (makeStmt el)
makeStmt  PSkip                 = Skip

-----------------------------------------------------------------------------------
collectUF :: ParseUF -> State ParseSt ()
-----------------------------------------------------------------------------------
collectUF (PUF v args) = st . ufs %= M.insert v args

-----------------------------------------------------------------------------------
updateTaintInfo :: St -> St
-----------------------------------------------------------------------------------
updateTaintInfo st = over irs mapIRs st
  where
    mapIRs :: [IR] -> [IR]
    mapIRs = map mapIR

    mapIR :: IR -> IR
    mapIR ir@(Always{..})     = ir
    mapIR ir@(ModuleInst{..}) = ir { modInstSt = fixSt modInstSt }

    fixSt :: St -> St 
    fixSt = set sources  (st^.sources) .
            set sinks    (st^.sinks) .
            set sanitize (st^.sanitize) .
            over irs mapIRs
        
  

