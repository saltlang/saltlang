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
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses #-}

-- | Precedence parsing.  This is a simple pass that applies the
-- syntax directives in each scope to parse 'Seq'-based expressions
-- into 'Apply'-based forms.
--
-- The basic grammar for expressions looks like this:
--
-- > start : exp
-- >       | partial_right
-- >       | partial_left
-- >
-- > exp : atom
-- >     | exp exp
-- >     | exp postfix
-- >     | exp infix exp
-- >
-- > partial_right : infix exp
-- >               | partial_right infix exp
-- >
-- > partial_left : exp infix
-- >              | exp infix partial_left
--
-- 'Exp's of kind 'Sym' are turned into infix or postfix terminals in
-- the grammar if they have syntax directives indicating that is their
-- fixity.  All other 'Exp's are turned into atoms.
--
-- Prior to parsing, the precedence parser extracts all syntax
-- directives from the einter program and constructs a graph of all
-- precedence relationships (cyclic precedence order relationships are
-- reported as errors).  Any symbol not having any syntax directives
-- or any expression that isn't a symbol is given the default prefix
-- precedence level.  Any infix symbol without precedence
-- relationships is given the default infix precedence level.
--
-- By virtue of the precedence graph, we can compare any two
-- productions to see if one has lower precedence than the other.
-- Additionally, we have associativity information for any infix
-- operator.  Therefore, the grammar above becomes unambiguous by
-- virtue of this information.
--
-- The parser itself is a variation on Simple Precedence Parsing,
-- modified to support the more complex precedence levels.
module Language.Salt.Surface.Precedence(
       precedence
       ) where

import Control.Monad
import Control.Monad.Messages.Class
import Control.Monad.Reader(MonadReader(..), ReaderT, runReaderT)
import Control.Monad.Refs.Class
import Control.Monad.Symbols.Class
import Control.Monad.Trans
import Data.Equivs.Monad(Equivs)
import Data.Graph.Inductive.Graph hiding (out)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.TransClos
import Data.HashTable.IO(BasicHashTable)
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
import qualified Data.IntMap as IntMap

data Production =
    -- | A right-partial call (ex. (+ 1)
    PartialRight {
      rightAfters :: ![Exp Apply Ref],
      rightArg :: !(Exp Apply Ref),
      rightOp :: !Ref,
      rightPos :: !Position
    }
    -- | A left-partial call (ex. (+ 1)
  | PartialLeft {
      leftBefores :: ![Exp Apply Ref],
      leftArg :: !(Exp Apply Ref),
      leftOp :: !Ref,
      leftPos :: !Position
    }
  | InfixOp {
      infixOp :: !Ref,
      infixOpPos :: !Position
    }
  | PostfixOp {
      postfixOp :: !Ref,
      postfixOpPos :: !Position
    }
  | Val {
      valExp :: !(Exp Apply Ref),
      valPrec :: !(Level Ref)
    }

data ParserData =
  ParserData {
    -- | A map of fixities for all 'Ref's.  Any definition rot
    -- represented has prefix fixity.
    parserFixities :: !(BasicHashTable Ref Fixity),
    -- | A map from 'Ref's to 'Node's in the graph.
    parserNodeMap :: !(BasicHashTable (Level Ref) Node),
    -- | A graph, where an edge from @a@ to @b@ means @a@ has lower
    -- precedence than @b@.
    parserPrecs :: !(Gr () ())
  }

instance PositionElement Production where
  position PartialRight { rightPos = pos } = pos
  position PartialLeft { leftPos = pos } = pos
  position InfixOp { infixOpPos = pos } = pos
  position PostfixOp { postfixOpPos = pos } = pos
  position Val { valExp = ex } = position ex

type ParserDataT m = ReaderT ParserData m

runParserDataT :: Monad m =>
                  ParserDataT m a
               -> ParserData
               -> m a
runParserDataT = runReaderT

defaultPos :: Position
defaultPos = Synthetic { synthDesc = "default precedence levels" }

defaultPosSet :: HashSet Position
defaultPosSet = HashSet.singleton defaultPos

defaultPrefix :: Level Ref
defaultPrefix = DefaultPrefix { prefixPos = defaultPos }

defaultInfix :: Level Ref
defaultInfix = DefaultInfix { infixPos = defaultPos }

-- | Get the fixity of an expression.  All expressions other than
-- 'Ref's have prefix fixity
production :: (MonadIO m, MonadReader ParserData m) =>
              Exp Apply Ref
           -> m Production
-- For a 'Sym', look up the syntax directive
production ex @ Sym { symRef = ref, symPos = pos } =
  do
    ParserData { parserFixities = fixities } <- ask
    res <- liftIO (HashTable.lookup fixities ref)
    case res of
      -- For postfix and infix, make the symbol an operator
      Just Postfix -> return PostfixOp { postfixOp = ref, postfixOpPos = pos }
      Just Infix {} -> return InfixOp { infixOp = ref, infixOpPos = pos }
      -- Prefix symbols are just atoms.
      Just Prefix -> return Val { valPrec = Level { levelRef =  ref },
                                  valExp = ex }
      -- By default, everything is an atom.
      Nothing -> return Val { valPrec = Level { levelRef =  ref },
                              valExp = ex }
-- Everything else is a val with default prefix precedence.
production ex =
  let
    pos = position ex
  in
    return Val { valPrec = DefaultPrefix { prefixPos = pos }, valExp = ex }

-- | Get the precedence level for a production.
level :: Production
      -- ^ The production for whichh to get the level
      -> Level Ref
level PartialRight { rightOp = op } = Level { levelRef = op }
level PartialLeft { leftOp = op } = Level { levelRef = op }
level InfixOp { infixOp = op } = Level { levelRef = op }
level PostfixOp { postfixOp = op } = Level { levelRef = op }
level Val { valPrec = lvl } = lvl

-- | Get the fixity of the Level
fixity :: (MonadReader ParserData m, MonadIO m) =>
          Level Ref
       -> m Fixity
fixity Level { levelRef = ref } =
  do
    ParserData { parserFixities = fixities } <- ask
    fix <- liftIO (HashTable.lookup fixities ref)
    case fix of
      -- No entry means prefix
      Nothing -> return Prefix
      Just out -> return out
fixity DefaultPrefix {} = return Prefix
-- Infix symbols are right-associative by default
fixity DefaultInfix {} = return Infix { infixAssoc = RightAssoc }

-- | Get the precedence graph node for a Level
node :: (MonadReader ParserData m, MonadIO m) =>
        Level Ref
     -> m Node
node lvl @ Level {} =
  do
    ParserData { parserNodeMap = nodemap } <- ask
    res <- liftIO (HashTable.lookup nodemap lvl)
    case res of
      Just out -> return out
      -- Fall back to the default level for the given fixity
      Nothing ->
        do
          fix <- fixity lvl
          case fix of
            Prefix -> node defaultPrefix
            Infix {} -> node defaultInfix
            Postfix -> error "Comparing postfix precedence"
-- Default levels should always have an entry in the node table
node lvl =
  do
    ParserData { parserNodeMap = nodemap } <- ask
    res <- liftIO (HashTable.lookup nodemap lvl)
    case res of
      Just out -> return out
      Nothing -> error "Default precedence levels should have a node"

-- | Check whether an equivalence class has lower precedence than
-- another equivalence class.
compareNodes :: (MonadReader ParserData m, MonadIO m) =>
                Node
             -- ^ The source node (the left-side equivalence class)
             -> Node
             -- ^ The destination node (the right-side equivalence class)
             -> m Bool
compareNodes left right =
  do
    ParserData { parserPrecs = precs } <- ask
    return (hasEdge precs (left, right))

-- | Parse a Seq into an Apply-based structure
parseExp :: (MonadMessages Message m, MonadSymbols m, MonadRefs m, MonadIO m) =>
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
  let
    composition :: MonadRefs m =>
                   Exp Apply Ref
                -> Exp Apply Ref
                -> m (Exp Apply Ref)
    composition left right =
      let
        pos = position left <> position right
        fields = IntMap.fromDistinctAscList [(1, left), (2, right)]
        tuple = Tuple { tupleFields = fields, tuplePos = pos }
      in do
        compref <- getComposeRef
        return Call { callInfo = Apply { applyFunc = Sym { symRef = compref,
                                                           symPos = pos },
                                         applyArg = tuple, applyPos = pos } }

    -- | Reduce the top of the stack, then contiune parsing
    reduce :: (MonadMessages Message m, MonadSymbols m,
               MonadRefs m, MonadIO m) =>
              [Exp Apply Ref]
           -- ^ The input stream
           -> [(Production, Maybe (Level Ref))]
           -- ^ The parser stack
           -> ParserDataT m (Exp Apply Ref)
    -- Infix call
    reduce input ((Val { valExp = right }, _) :
                  (InfixOp { infixOp = ref, infixOpPos = sympos }, _) :
                  (Val { valExp = left }, prev) :
                  rest) =
      let
        pos = position left <> position right
        func = Sym { symRef = ref, symPos = sympos }
        fields = IntMap.fromDistinctAscList [(1, left), (2, right)]
        tuple = Tuple { tupleFields = fields, tuplePos = pos }
        apply = Apply { applyFunc = func, applyArg = tuple, applyPos = pos }
        call = Call { callInfo = apply }
        val = Val { valExp = call, valPrec = Level { levelRef = ref } }
      in
        parse input ((val, prev) : rest)
    -- Left partial call concatenation
    reduce input ((PartialLeft { leftBefores = befores, leftOp = oldref,
                                 leftArg = oldleft, leftPos = oldpos }, _) :
                  (InfixOp { infixOp = ref, infixOpPos = oppos }, _) :
                  (Val { valExp = left }, prev) :
                  rest) =
      let
        pos = oppos <> position left
        fields = IntMap.singleton 1 oldleft
        tuple = Tuple { tupleFields = fields, tuplePos = oldpos }
        func = Sym { symRef = oldref, symPos = oldpos }
        with = With { withVal = func, withArgs = tuple, withPos = oldpos }
        val = PartialLeft { leftBefores = with : befores, leftOp = ref,
                            leftArg = left, leftPos = pos }
      in
        parse input ((val, prev) : rest)
    -- Right partial call concatenation
    reduce input ((Val { valExp = right }, _) :
                  (InfixOp { infixOp = ref, infixOpPos = oppos }, _) :
                  (PartialRight { rightAfters = afters, rightArg = oldright,
                                  rightOp = oldref, rightPos = oldpos }, prev) :
                  rest) =
      let
        pos = position right <> oppos
        fields = IntMap.singleton 2 oldright
        tuple = Tuple { tupleFields = fields, tuplePos = oldpos }
        func = Sym { symRef = oldref, symPos = oldpos }
        with = With { withVal = func, withArgs = tuple, withPos = oldpos }
        val = PartialRight { rightAfters = with : afters, rightOp = ref,
                             rightArg = right, rightPos = pos }
      in
        parse input ((val, prev) : rest)
    -- Postfix call
    reduce input ((PostfixOp { postfixOp = ref, postfixOpPos = pos }, _) :
                  (Val { valExp = arg }, prev) :
                  rest) =
      let
        func = Sym { symRef = ref, symPos = pos }
        apply = Apply { applyFunc = func, applyArg = arg,
                        applyPos = pos <> position arg }
        call = Call { callInfo = apply }
        val = Val { valExp = call, valPrec = defaultPrefix }
      in
        parse input ((val, prev) : rest)
    -- Prefix call
    reduce input ((Val { valExp = arg }, _) :
                  (Val { valExp = func, valPrec = prec }, prev) :
                  rest) =
      let
        apply = Apply { applyFunc = func, applyArg = arg,
                        applyPos = position func <> position arg }
        call = Call { callInfo = apply }
        val = Val { valExp = call, valPrec = prec }
      in
        parse input ((val, prev) : rest)
    -- Right partial call
    reduce input ((Val { valExp = right }, _) :
                  (InfixOp { infixOp = ref, infixOpPos = oppos }, prev) :
                  rest) =
      let
        pos = oppos <> position right
        val = PartialRight { rightAfters = [], rightOp = ref,
                             rightArg = right, rightPos = pos }
      in
        parse input ((val, prev) : rest)
    -- Left partial call
    reduce input ((InfixOp { infixOp = ref, infixOpPos = oppos }, _) :
                  (Val { valExp = left }, prev) :
                  rest) =
      let
        pos = oppos <> position left
        val = PartialLeft { leftBefores = [], leftOp = ref,
                            leftArg = left, leftPos = pos }
      in
        parse input ((val, prev) : rest)
    -- Reduce a partial right into a value
    reduce input ((PartialRight { rightAfters = afters, rightArg = right,
                                  rightOp = ref, rightPos = pos }, prev) :
                  rest) =
      let

        fields = IntMap.singleton 2 right
        tuple = Tuple { tupleFields = fields, tuplePos = pos }
        func = Sym { symRef = ref, symPos = pos }
        with = With { withVal = func, withArgs = tuple, withPos = pos }
        complist = with : afters
      in do
        composed <- foldM composition (last complist) (init complist)
        parse input ((Val { valExp = composed, valPrec = defaultPrefix },
                      prev) : rest)
    -- Reduce a partial left into a value
    reduce input ((PartialLeft { leftBefores = befores, leftOp = ref,
                                 leftArg = left, leftPos = pos }, prev) :
                  rest) =
      let
        fields = IntMap.singleton 1 left
        tuple = Tuple { tupleFields = fields, tuplePos = pos }
        func = Sym { symRef = ref, symPos = pos }
        with = With { withVal = func, withArgs = tuple, withPos = pos }
      in do
        composed <- foldM composition with (reverse befores)
        parse input ((Val { valExp = composed, valPrec = defaultPrefix },
                      prev) : rest)
    reduce _ _ =
      do
        internalError "Invalid parser stack state" [seqpos]
        return Bad { badPos = seqpos }

    -- | Control function for the precedence parser.
    parse :: (MonadMessages Message m, MonadSymbols m,
              MonadRefs m, MonadIO m) =>
             [Exp Apply Ref]
          -- ^ The input stream
          -> [(Production, Maybe (Level Ref))]
          -- ^ The parser stack
          -> ParserDataT m (Exp Apply Ref)
    -- Terminating condition: we have one Val element on the stack.
    parse [] [(Val { valExp = ex }, _)] = return ex
    -- If we run out of inputs, keep reducing the stack until we hit one Val.
    parse [] stack = reduce [] stack
    -- If the stack is empty, we always shift
    parse (first : rest) [] =
      do
        prod <- production first
        case prod of
          -- We can shift Val's or InfixOp's
          Val {} -> parse rest [(prod, Nothing)]
          InfixOp {} -> parse rest [(prod, Nothing)]
          -- Anything else is a parse error
          _ ->
            let
              pos = position first
            in do
              precedenceParseError pos
              return Bad { badPos = pos }
    -- Otherwise, determine the action by comparing precedence
    parse input @ (first : rest) stack @ ((top, curr) : _) =
      let
        err :: MonadMessages Message m =>
                      m (Exp Apply Ref)
        err =
          let
            pos = position first
          in do
            precedenceParseError pos
            return Bad { badPos = pos }

        -- Push a production on to the stack
        shift prod =
          let
            newlvl = level top
          in
            parse rest ((prod, Just newlvl) : stack)

        -- | Decide what to do based on the precedence of the input
        -- vs. the current precedence level.
        decide prod =
          case (level prod, curr) of
            -- Current level of "nothing" means that there is only one
            -- production on the stack, so shift.
            (_, Nothing) -> shift prod
            -- If we have the same reference, then decide what to do based
            -- on associativity
            (lvl @ Level { levelRef = prodref },
             Just Level { levelRef = currref })
              | prodref == currref ->
                do
                  fix <- fixity lvl
                  case fix of
                    -- Prefix symbols are left-associative, reduce.
                    Prefix -> reduce input stack
                    Infix { infixAssoc = LeftAssoc } -> reduce input stack
                    Infix { infixAssoc = RightAssoc } -> shift prod
                    -- Non-associative symbols produce a parse error when
                    -- compared for associativity
                    Infix { infixAssoc = NonAssoc } -> err
                    -- This shouldn't happen
                    Postfix ->
                      let
                        pos = position first
                      in do
                        internalError "Comparing postfix precedences" [pos]
                        return Bad { badPos = pos }
            -- Otherwise, look up in the precedenece graph
            (prodlvl, Just currlvl) ->
              do
                -- Look up the nodes (equivalence classes) for the input
                -- and current precedence levels
                prodnode <- node prodlvl
                currnode <- node currlvl
                -- Check the graph to see if the input has lower precedence
                res <- compareNodes prodnode currnode
                -- If the input has lower precedence, then reduce,
                -- otherwise, shift
                if res
                  then reduce input stack
                  else shift prod
      in do
        prod <- production first
        case (top, prod) of
          -- For these, we might be shifting, or we might be reducing,
          -- depending on the precedence state.
          (Val {}, Val {}) -> decide prod
          (Val {}, PostfixOp {}) -> decide prod
          (Val {}, InfixOp {}) -> decide prod
          -- A postfix always gets reduced immediately
          (PostfixOp {}, _) -> reduce input stack
          -- We always shift a value after the infix operator
          (InfixOp {}, Val {}) -> shift prod
          -- Always shift an infix operator after a partial call
          (PartialRight {}, InfixOp {}) -> shift prod
          -- Anything else is a parse error
          (_, _) -> err
  in do
    newexps <- mapM doExp exps
    parse newexps []

-- | Convert an 'Exp' based on 'Seq's to one based on 'Apply'.
doExp :: (MonadMessages Message m, MonadSymbols m, MonadRefs m, MonadIO m) =>
         Exp Seq Ref
      -> ParserDataT m (Exp Apply Ref)
-- Calls invoke the parser
doExp Call { callInfo = info } = parseExp info
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
scanScope :: (MonadMessages Message m, MonadIO m) =>
             Equivs (Level Ref) (HashSet Position)
          -- ^ The 'Equivs' structure into which to put all the
          -- equivalences.
          -> BasicHashTable (Level Ref, Level Ref) (HashSet Position)
          -- ^ A hash table into which to gather up all the edges.
          -> BasicHashTable Ref Fixity
          -- ^ A hash table into which to gather up all fixities.
          -> (ScopeID, Scope (Exp Seq Ref))
          -- ^ The scope to scan.
          -> m ()
scanScope equivs edgetab fixities (scopeid, Scope { scopeSyntax = syntax }) =
  let
    -- | Scan all precedence directives for equalities, add those
    -- to the equivalence set.
    scanSyntax :: (MonadMessages Message m, MonadIO m) =>
                  (Symbol, Syntax (Exp Seq Ref))
               -- ^ The syntax directive to scan
               -> m ()
    scanSyntax (sym, Syntax { syntaxFixity = fix, syntaxPrecs = precs,
                              syntaxPos = synpos }) =
      let
        currref = Ref { refSymbol = sym, refScopeID = scopeid }
        currlvl = Level { levelRef = currref }

        -- | Add an edge to the edge table.
        addEdge :: Level Ref
                -- ^ The source.
                -> Level Ref
                -- ^ The destination.
                -> IO ()
        addEdge src dst =
          let
            key = (src, dst)
          in do
            res <- HashTable.lookup edgetab key
            case res of
              Just posset ->
                HashTable.insert edgetab key (HashSet.insert synpos posset)
              Nothing -> HashTable.insert edgetab key (HashSet.singleton synpos)

        -- | Scan a single precedence directive.
        scanPrec :: (MonadMessages Message m, MonadIO m) =>
                    Prec (Exp Seq Ref)
                 -- ^ The syntax directive to scan
                 -> m ()
        -- Add an equivalence if the precedence relation is equality

        -- We expect to see a reference, or else one of the default
        -- precedence levels.
        scanPrec Prec { precLevel = Level { levelRef = Sym { symRef = ref } },
                        precOrd = EQ, precPos = pos } =
          let
            lvl = Level { levelRef = ref }
          in
            liftIO (Equivs.addEquiv equivs currlvl lvl (HashSet.singleton pos))
        scanPrec Prec { precLevel = DefaultPrefix { prefixPos = lvlpos },
                        precOrd = EQ, precPos = pos } =
          let
            lvl = DefaultPrefix { prefixPos = lvlpos }
          in
            liftIO (Equivs.addEquiv equivs currlvl lvl (HashSet.singleton pos))
        scanPrec Prec { precLevel = DefaultInfix { infixPos = lvlpos },
                        precOrd = EQ, precPos = pos } =
          let
            lvl = DefaultInfix { infixPos = lvlpos }
          in
            liftIO (Equivs.addEquiv equivs currlvl lvl (HashSet.singleton pos))

        -- For LT, we add an edge to the edge set.  Also, add
        -- the target as a singleton equivalence class.

        -- We expect to see a reference, or else one of the default
        -- precedence levels.
        scanPrec Prec { precLevel = Level { levelRef = Sym { symRef = ref } },
                        precOrd = LT, precPos = pos } =
          let
            lvl = Level { levelRef = ref }
          in do
            liftIO (addEdge currlvl lvl)
            -- Make sure the target is present in the equivalence
            -- structure
            liftIO (Equivs.addSingle equivs lvl (HashSet.singleton pos))
        scanPrec Prec { precLevel = DefaultPrefix { prefixPos = lvlpos },
                        precOrd = LT, precPos = pos } =
          let
            lvl = DefaultPrefix { prefixPos = lvlpos }
          in do
            liftIO (addEdge currlvl lvl)
            -- Make sure the target is present in the equivalence
            -- structure
            liftIO (Equivs.addSingle equivs lvl (HashSet.singleton pos))
        scanPrec Prec { precLevel = DefaultInfix { infixPos = lvlpos },
                        precOrd = LT, precPos = pos } =
          let
            lvl = DefaultInfix { infixPos = lvlpos }
          in do
            liftIO (addEdge currlvl lvl)
            -- Make sure the target is present in the equivalence
            -- structure
            liftIO (Equivs.addSingle equivs lvl (HashSet.singleton pos))

        -- For GT, we do the same thing, but flip the edge direction.

        -- We expect to see a reference, or else one of the default
        -- precedence levels.
        scanPrec Prec { precLevel = Level { levelRef = Sym { symRef = ref } },
                        precOrd = GT, precPos = pos } =
          let
            lvl = Level { levelRef = ref }
          in do
            liftIO (addEdge lvl currlvl)
            -- Make sure the target is present in the equivalence
            -- structure
            liftIO (Equivs.addSingle equivs lvl (HashSet.singleton pos))
        scanPrec Prec { precLevel = DefaultPrefix { prefixPos = lvlpos },
                        precOrd = GT, precPos = pos } =
          let
            lvl = DefaultPrefix { prefixPos = lvlpos }
          in do
            liftIO (addEdge lvl currlvl)
            -- Make sure the target is present in the equivalence
            -- structure
            liftIO (Equivs.addSingle equivs lvl (HashSet.singleton pos))
        scanPrec Prec { precLevel = DefaultInfix { infixPos = lvlpos },
                        precOrd = GT, precPos = pos } =
          let
            lvl = DefaultInfix { infixPos = lvlpos }
          in do
            liftIO (addEdge lvl currlvl)
            -- Make sure the target is present in the equivalence
            -- structure
            liftIO (Equivs.addSingle equivs lvl (HashSet.singleton pos))

        -- For anything else, we expected a reference, but didn't get
        -- one, so report an error.
        scanPrec Prec { precLevel = lvl } = expectedRef (position lvl)
      in do
        -- Make sure the current reference is present in the
        -- equivalence structure
        liftIO (Equivs.addSingle equivs currlvl HashSet.empty)
        -- Add the fixity to the fixity table
        liftIO (HashTable.insert fixities currref fix)
        -- Scan the precedence directives
        mapM_ scanPrec precs
  in
    mapM_ scanSyntax (HashMap.toList syntax)

-- | Transform 'Seq's into 'Apply's using the grammar derived from the
-- syntax directives in each scope.
precedence :: (MonadMessages Message m, MonadSymbols m,
               MonadRefs m, MonadIO m) =>
              Surface (Exp Seq Ref)
           -- ^ Surface syntax structure to transform.
           -> m (Surface (Exp Apply Ref))
           -- ^ Transformed surface syntax structure.
precedence surface @ Surface { surfaceScopes = scopes } =
  let
    buildGraph :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                  [((Level Ref, Level Ref), HashSet Position)]
               -> ([(Level Ref, Node)], [(Node, HashSet Position)])
               -> m (Gr (HashSet Position) (HashSet Position),
                     BasicHashTable (Level Ref) Node)
    buildGraph edgelist (entries, nodelist) =
      let
        -- | Convert edge table entries into edges for the graph.
        foldfun :: (MonadMessages Message m, MonadSymbols m, MonadIO m) =>
                   BasicHashTable (Level Ref) Node
                -- ^ Node mapping table.
                -> [(Node, Node, HashSet Position)]
                -- ^ List of edges.
                -> ((Level Ref, Level Ref), HashSet Position)
                -- ^ Edge table entry.
                -> m [(Node, Node, HashSet Position)]
                -- ^ New list of edges.
        foldfun nodemap accum ((src, dst), pos) =
          do
            srcres <- liftIO (HashTable.lookup nodemap src)
            dstres <- liftIO (HashTable.lookup nodemap dst)
            case (srcres, dstres) of
              (Just srcnode, Just dstnode) ->
                return ((srcnode, dstnode, pos) : accum)
              _ ->
                do
                  internalError "No node mapping for reference"
                                (HashSet.toList pos)
                  return accum

      in do
        nodemap <- liftIO (HashTable.fromList entries)
        newedges <- foldM (foldfun nodemap) [] edgelist
        return (mkGraph nodelist newedges, nodemap)

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
        mapfun subnodes =
          let
            -- Take the subgraph
            sccgraph = subgraph subnodes graph
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
        mapfun ((refs, pos), nodeid) = (zip refs (repeat nodeid), (nodeid, pos))
        ascending = iterate (+1) 0
        contents classes =
          let
            (maplist, nodelist) = unzip (map mapfun (zip classes ascending))
          in
            (concat maplist, nodelist)
      in do
        -- Create Equivalence structure
        equivs <- liftIO Equivs.new
        -- Add default prefix and infix entries
        liftIO (Equivs.addSingle equivs defaultPrefix defaultPosSet)
        liftIO (Equivs.addSingle equivs defaultInfix defaultPosSet)
        -- Create edges structure
        edgetab <- liftIO HashTable.new
        -- The default prefix level has lower precedence than the
        -- default infix level
        liftIO (HashTable.insert edgetab (defaultPrefix, defaultInfix)
                                       defaultPosSet)
        fixities <- liftIO HashTable.new
        -- Scan all scopes, extracting the syntax information
        mapM_ (scanScope equivs edgetab fixities) (Array.assocs scopes)
        -- Get the equivalence classes and edges
        classes <- liftIO (Equivs.toEquivs equivs)
        edgelist <- liftIO (HashTable.toList edgetab)
        -- Convert information into a graph
        (graph, nodemap) <- buildGraph edgelist (contents classes)
        -- Check that the graph forms a partial order.
        unlabeled <- checkGraph graph
        -- Finally, compute the reachability graph so we can compare
        -- just by checking if an edge exists.
        return ParserData { parserPrecs = tc unlabeled,
                            parserNodeMap = nodemap,
                            parserFixities = fixities }

  in do
    -- First, build equivalence classes for all the equality
    -- precedence directives.
    parserinfo <- buildParserInfo
    -- Once we have the parsing data structure, descend through the
    -- structure and use it to parse all Seq's.
    runParserDataT (mapM doExp surface) parserinfo
