-- Copyright (c) 2016 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses #-}

-- | Precedence parsing.  This is a simple pass that applies the
-- syntax directives in each scope to parse 'Seq'-based expressions
-- into 'Apply'-based forms.
module Language.Salt.Surface.Precedence(
       precedence
       ) where

import Control.Monad
import Control.Monad.Messages.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Symbols.Class
import Control.Monad.Trans
import Data.Equivs.Monad(Equivs)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Data.HashTable.IO(BasicHashTable)
import Data.HashMap.Strict(HashMap)
import Data.HashSet(HashSet)
import Data.PositionElement
import Data.ScopeID
import Data.Semigroup((<>))
import Data.Symbol
import Language.Salt.Message
import Language.Salt.Surface.Common
import Language.Salt.Surface.Syntax

import qualified Data.Array as Array
import qualified Data.Equivs.Monad as Equivs
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.HashTable.IO as HashTable

data ParserData =
  ParserData {
    -- | A map of fixities for all 'Ref's.  Any definition rot
    -- represented has prefix fixity.
    parserFixities :: !(BasicHashTable Ref Fixity),
    -- | A map from 'Ref's to 'Node's in the graph.
    parserNodeMap :: !(BasicHashTable Ref Node),
    -- | A graph, where an edge from @a@ to @b@ means @a@ has lower
    -- precedence than @b@.
    parserPrecs :: !(Gr () ())
  }

data PDAState =
  PDAState {
    pdaStack :: ![(Ordering, Production)]
  }

data Production =
    -- | A right-partial call (ex. (+ 1)
    PartialRight {
      rightArg :: !(Exp Apply Ref),
      rightOp :: !Ref,
      rightPos :: !Position
    }
    -- | A left-partial call (ex. (+ 1)
  | PartialLeft {
      leftArg :: !(Exp Apply Ref),
      leftOp :: !Ref,
      leftPos :: !Position
    }
  | InfixOp {
      infixOp :: !Ref,
      infixPos :: !Position
    }
  | PostfixOp {
      postfixOp :: !Ref,
      postfixPos :: !Position
    }
  | Atom { atomExp :: !(Exp Apply Ref) }

instance PositionElement Production where
  position PartialRight { rightPos = pos } = pos
  position PartialLeft { leftPos = pos } = pos
  position InfixOp { infixPos = pos } = pos
  position PostfixOp { postfixPos = pos } = pos
  position Atom { atomExp = ex } = position ex

type ParserDataT m = ReaderT ParserData m
type ParserT m = StateT PDAState m

runParserDataT :: Monad m =>
                  ParserDataT m a
               -> ParserData
               -> m a
runParserDataT = runReaderT

-- | Get the fixity of an expression.  All expressions other than
-- 'Ref's have prefix fixity
production :: (MonadIO m, MonadReader ParserData m) =>
              Exp Apply Ref
           -> m Production
-- For a 'Sym', look up the syntax directive
production ex @ Sym { symRef = ref, symPos = pos } =
  do
    ParserData { parserFixities = fixity } <- ask
    res <- liftIO (HashTable.lookup fixity ref)
    case res of
      -- For postfix and infix, make the symbol an operator
      Just Postfix -> return PostfixOp { postfixOp = ref, postfixPos = pos }
      Just Infix {} -> return InfixOp { infixOp = ref, infixPos = pos }
      -- Prefix symbols are just atoms.
      Just Prefix -> return Atom { atomExp = ex }
      -- By default, everything is an atom.
      Nothing -> return Atom { atomExp = ex }
-- Everything else is an atom.
production ex = return Atom { atomExp = ex }

comparePrec :: (MonadMessages Message m, MonadSymbols m,
                MonadReader ParserData m, MonadIO m) =>
               Production
            -> Production
            -> m Ordering
comparePrec left right = _

reduce :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
          [(Production, Ordering)]
       -> ParserDataT m [(Production, Ordering)]
-- Throw an error here; we will loop forever on this
reduce [] = error "Cannot reduce empty stack"
reduce stack =
  let
    (pivot, rest) = span ((== LT) . snd) stack

    push prod = case rest of
      [] -> return [(prod, LT)]
      ((top, _) : _) ->
        do
          ord <- comparePrec prod top
          return ((prod, ord) : rest)
  in
    case pivot of
      -- Throw an error here; we will loop forever on these
      [] -> error "Empty pivot should not happen"
      [(Atom {}, _)] -> error "Attempt to reduce a single atom"
      -- Turn partial calls into atoms
      [(PartialRight {}, _)] -> _
      [(PartialLeft {}, _)] -> _
      -- Postfix call
      [(PostfixOp { postfixOp = ref, postfixPos = pos }, _),
       (Atom { atomExp = arg }, _)] ->
        let
          func = Sym { symRef = ref, symPos = pos }
          apply = Apply { applyFunc = func, applyArg = arg,
                          applyPos = pos <> position arg }
          call = Call { callInfo = apply }
        in
          push Atom { atomExp = call }
      -- Prefix call
      [(Atom { atomExp = arg }, _),
       (Atom { atomExp = func }, _)] ->
        let
          apply = Apply { applyFunc = func, applyArg = arg,
                          applyPos = position func <> position arg }
          call = Call { callInfo = apply }
        in
          push Atom { atomExp = call }
      -- Right partial call
      [(Atom { atomExp = right }, _),
       (InfixOp { infixOp = ref, infixPos = oppos }, _)] ->
        let
          pos = oppos <> position right
        in
          push PartialRight { rightOp = ref, rightArg = right, rightPos = pos }
      -- Left partial call
      [(InfixOp { infixOp = ref, infixPos = oppos }, _),
       (Atom { atomExp = left }, _)] ->
        let
          pos = oppos <> position left
        in
          push PartialRight { rightOp = ref, rightArg = left, rightPos = pos }
      -- Infix call
      [(Atom { atomExp = right }, _),
       (InfixOp { infixOp = ref, infixPos = sympos }, _),
       (Atom { atomExp = left }, _)] ->
        let
          pos = position left <> position right
          func = Sym { symRef = ref, symPos = pos }
          tuple = Tuple { tupleFields = [left, right], tuplePos = pos }
          apply = Apply { applyFunc = func, applyArg = tuple, applyPos = pos }
          call = Call { callInfo = apply }
        in
          push Atom { atomExp = call }
      -- Partial call concatenation
      [(PartialLeft {}, _),
       (InfixOp { infixOp = ref, infixPos = sympos }, _),
       (Atom { atomExp = left }, _)] -> _
      [(Atom { atomExp = right }, _),
       (InfixOp { infixOp = ref, infixPos = sympos }, _),
       (PartialRight {}, _)] -> _
      -- Anything else is a parse error
      _ ->
        let
          pos = position (fst (last pivot)) <> position (fst (head pivot))
        in do
          -- Report the error and push a bad production
          precedenceParseError pos
          push Atom { atomExp = Bad { badPos = pos } }


-- | Control function for the simple precedence parser.
parse :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
         [Exp Apply Ref]
      -- ^ The input stream
      -> [(Production, Ordering)]
      -- ^ The parser stack
      -> ParserDataT m (Exp Apply Ref)
-- Terminating condition: we have one Atom element on the stack.
parse [] [(Atom { atomExp = exp }, _)] = return exp
-- If we run out of inputs, keep reducing the stack until we hit one Atom.
parse [] stack =
  do
    newstack <- reduce stack
    parse [] newstack
-- If the stack is empty, we always shift
parse (first : rest) [] =
  do
    prod <- production first
    parse rest [(prod, LT)]
-- Otherwise, determine the action by comparing precedence
parse input @ (first : rest) stack @ ((top, _) : _) =
  do
    prod <- production first
    ord <- comparePrec prod top
    case ord of
      GT ->
        do
          newstack <- reduce stack
          parse input newstack
      -- Otherwise, shift
      _ -> parse rest ((prod, ord) : stack)


-- | Parse a Seq into an Apply-based structure
parseExp :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
            Seq Ref
         -> ParserDataT m (Exp Apply Ref)
-- Report errors for empty and singleton lists
parseExp Seq { seqExps = [], seqPos = seqpos } =
  do
    internalError "Should not see empty list in Seq" [seqpos]
    return Bad { badPos = seqpos }
parseExp Seq { seqExps = [_], seqPos = seqpos } =
  do
    internalError "Should not see singeton list in Seq" [seqpos]
    return Bad { badPos = seqpos }
parseExp Seq { seqExps = exps, seqPos = seqpos } =
  do
    newexps <- mapM doExp exps
    parse newexps []

-- | Convert an 'Exp' based on 'Seq's to one based on 'Apply'.
doExp :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
         Exp Seq Ref
      -> ParserDataT m (Exp Apply Ref)
-- Calls invoke the parser
doExp c @ Call { callInfo = info } = parseExp info
-- Everything else is constructive
doExp c @ Compound { compoundBody = body } =
  do
    newbody <- mapM (mapM doExp) body
    return c { compoundBody = newbody }
doExp a @ Abs { absCases = cases } =
  do
    newcases <- mapM (mapM doExp) cases
    return a { absCases = newcases }
doExp m @ Match { matchVal = val, matchCases = cases } =
  do
    newval <- doExp val
    newcases <- mapM (mapM doExp) cases
    return m { matchVal = newval, matchCases = newcases }
doExp a @ Ascribe { ascribeVal = val, ascribeType = ty } =
  do
    newval <- doExp val
    newty <- doExp ty
    return a { ascribeVal = newval, ascribeType = newty }
doExp r @ RecordType { recordTypeFields = fields } =
  do
    newfields <- mapM doExp fields
    return r { recordTypeFields = newfields }
doExp r @ Record { recordFields = fields } =
  do
    newfields <- mapM doExp fields
    return r { recordFields = newfields }
doExp t @ Tuple { tupleFields = fields } =
  do
    newfields <- mapM doExp fields
    return t { tupleFields = newfields }
doExp p @ Project { projectVal = val } =
  do
    newval <- doExp val
    return p { projectVal = newval }
doExp w @ With { withVal = val, withArgs = args } =
  do
    newval <- doExp val
    newargs <- doExp args
    return w { withVal = newval, withArgs = newargs }
doExp w @ Where { whereVal = val, whereProp = prop } =
  do
    newval <- doExp val
    newprop <- doExp prop
    return w { whereVal = newval, whereProp = newprop }
doExp a @ Anon { anonSuperTypes = supers, anonParams = params } =
  do
    newsupers <- mapM doExp supers
    newparams <- mapM doExp params
    return a { anonSuperTypes = newsupers, anonParams = newparams }
-- These two have nothing to convert
doExp Sym { symRef = ref, symPos = pos } =
  return Sym { symRef = ref, symPos = pos }
doExp Literal { literalVal = val } = return Literal { literalVal = val }
doExp Bad { badPos = pos } = return Bad { badPos = pos }

-- | Build precedence equivalence classes for all symbols.
scanScope :: Equivs Ref (HashSet Position)
          -- ^ The 'Equivs' structure into which to put all the
          -- equivalences.
          -> BasicHashTable (Ref, Ref) (HashSet Position)
          -- ^ A hash table into which to gather up all the edges.
          -> BasicHashTable Ref Fixity
          -- ^ A hash table into which to gather up all fixities.
          -> (ScopeID, Scope (Exp Seq Ref))
          -- ^ The scope to scan.
          -> IO ()
scanScope equivs edges fixities (scopeid, Scope { scopeSyntax = syntax }) =
  let
    -- | Scan all precedence directives for equalities, add those
    -- to the equivalence set.
    scanSyntax :: (Symbol, Syntax (Exp Seq Ref))
               -- ^ The syntax directive to scan
               -> IO ()
    scanSyntax (sym, Syntax { syntaxFixity = fixity, syntaxPrecs = precs,
                              syntaxPos = pos }) =
      let
        currref = Ref { refSymbol = sym, refScopeID = scopeid }

        -- | Add an edge to the edge table.
        addEdge :: Ref
                -- ^ The source.
                -> Ref
                -- ^ The destination.
                -> IO ()
        addEdge src dst =
          let
            key = (src, dst)
          in do
            res <- HashTable.lookup edges key
            case res of
              Just posset ->
                HashTable.insert edges key (HashSet.insert pos posset)
              Nothing -> HashTable.insert edges key (HashSet.singleton pos)

        -- | Scan a single precedence directive.
        scanPrec :: Prec (Exp Seq Ref)
                 -- ^ The syntax directive to scan
                 -> IO ()
        -- Add an equivalence if the precedence relation is equality
        scanPrec Prec { precExp = Sym { symRef = ref },
                        precOrd = EQ, precPos = pos } =
          Equivs.addEquiv equivs currref ref (HashSet.singleton pos)
        -- Otherwise, we add an edge to the edge set.  Also, add the
        -- target as a singleton equivalence class.
        scanPrec Prec { precExp = Sym { symRef = ref },
                        precOrd = LT, precPos = pos } =
          do
            addEdge currref ref
            -- Make sure the target is present in the equivalence
            -- structure
            Equivs.addSingle equivs ref HashSet.empty
        -- For GT, we do the same thing, but flip the
        scanPrec Prec { precExp = Sym { symRef = ref },
                        precOrd = GT, precPos = pos } =
          do
            addEdge ref currref
            -- Make sure the target is present in the equivalence
            -- structure
            Equivs.addSingle equivs ref HashSet.empty
      in do
        -- Make sure the current reference is present in the
        -- equivalence structure
        Equivs.addSingle equivs currref HashSet.empty
        -- Add the fixity to the fixity table
        HashTable.insert fixities currref fixity
        -- Scan the precedence directives
        mapM_ scanPrec precs
  in
    mapM_ scanSyntax (HashMap.toList syntax)

-- | Transform 'Seq's into 'Apply's using the grammar derived from the
-- syntax directives in each scope.
precedence :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
              Surface (Exp Seq Ref)
           -- ^ Surface syntax structure to transform.
           -> m (Surface (Exp Apply Ref))
           -- ^ Transformed surface syntax structure.
precedence s @ Surface { surfaceScopes = scopes } =
  let
    buildGraph :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                  [((Ref, Ref), HashSet Position)]
               -> ([(Ref, Node)], [(Node, HashSet Position)])
               -> m (Gr (HashSet Position) (HashSet Position),
                     BasicHashTable Ref Node)
    buildGraph edges (entries, nodes) =
      let
        -- | Convert edge table entries into edges for the graph.
        foldfun :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                   BasicHashTable Ref Node
                -- ^ Node mapping table.
                -> [(Node, Node, HashSet Position)]
                -- ^ List of edges.
                -> ((Ref, Ref), HashSet Position)
                -- ^ Edge table entry.
                -> m [(Node, Node, HashSet Position)]
                -- ^ New list of edges.
        foldfun nodemap edgelist ((src, dst), pos) =
          do
            srcres <- liftIO (HashTable.lookup nodemap src)
            dstres <- liftIO (HashTable.lookup nodemap dst)
            case (srcres, dstres) of
              (Just srcnode, Just dstnode) ->
                return ((srcnode, dstnode, pos) : edgelist)
              _ ->
                do
                  internalError "No node mapping for reference"
                                (HashSet.toList pos)
                  return edgelist

      in do
        nodemap <- liftIO (HashTable.fromList entries)
        edgelist <- foldM (foldfun nodemap) [] edges
        return (mkGraph nodes edgelist, nodemap)

    -- | Check that the graph forms a partial order.  Return the same
    -- graph with the positions discarded.
    checkGraph :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                  Gr (HashSet Position) (HashSet Position)
               -> m (Gr () ())
    checkGraph graph =
      let
        -- This shouldn't happen
        mapfun :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                  [Node] -> m ()
        mapfun [] = internalError "Empty SCC element list" []
        -- Single-element SCC's are OK
        mapfun [_] = return ()
        -- Multi-element SCC's denote cyclic precedence relationships
        mapfun nodes =
          let
            -- Take the subgraph
            sccgraph = subgraph nodes graph
            -- Get every position in the position sets
            nodelist = labNodes sccgraph
            edgelist = labEdges sccgraph
            poslist = map snd nodelist ++ map (\(_, _, p) -> p) edgelist
            posset = mconcat poslist
          in
            cyclicPrecedence (HashSet.toList posset)
      in do
        mapM_ mapfun (scc graph)
        return (mkUGraph (map fst (labNodes graph))
                         (map (\(s, d, _) -> (s, d)) (labEdges graph)))

    -- | Scan all scopes for equality precedence directives, build
    -- equivalence classes over all those symbols, map those to 'Node' IDs
    -- for the graph.
    buildParserInfo :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                       m ParserData
    buildParserInfo =
      let
        mapfun ((refs, pos), node) = (zip refs (repeat node), (node, pos))
        ascending = iterate (+1) 0
        contents classes =
          let
            (maplist, nodes) = unzip (map mapfun (zip classes ascending))
          in
            (concat maplist, nodes)
      in do
        -- Create tables
        equivs <- liftIO Equivs.new
        edges <- liftIO HashTable.new
        fixities <- liftIO HashTable.new
        -- Scan all scopes, extracting the syntax information
        liftIO (mapM_ (scanScope equivs edges fixities) (Array.assocs scopes))
        -- Get the equivalence classes and edges
        classes <- liftIO (Equivs.toEquivs equivs)
        edgelist <- liftIO (HashTable.toList edges)
        -- Convert information into a graph
        (graph, nodemap) <- buildGraph edgelist (contents classes)
        -- Check that the graph forms a partial order.
        unlabeled <- checkGraph graph
        return ParserData { parserPrecs = unlabeled,
                            parserNodeMap = nodemap,
                            parserFixities = fixities }

  in do
    -- First, build equivalence classes for all the equality
    -- precedence directives.
    parserinfo <- buildParserInfo
    -- Once we have the parsing data structure, descend through the
    -- structure and use it to parse all Seq's.
    runParserDataT (mapM doExp s) parserinfo
