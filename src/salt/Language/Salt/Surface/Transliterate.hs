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

module Language.Salt.Surface.Transliterate(
       transliterate
       ) where

import Bound.Scope
import Control.Monad.Positions.Class
import Control.Monad.State
import Data.HashSet(HashSet)
import Data.Position.BasicPosition(BasicPosition)
import Data.Position.DWARFPosition(DWARFPosition)
import Data.PositionElement
import Data.Symbol

import qualified Data.HashSet as HashSet
import qualified Data.Position.BasicPosition as Basic
import qualified Data.Position.DWARFPosition as DWARF
import qualified Language.Salt.Core.Syntax as Core
import qualified Language.Salt.Surface.Syntax as Surface
import qualified Language.Salt.Surface.Common as Surface

data TransState =
  TransState {
    transPositionStack :: ![Surface.Position -> Core.Position],
    transPosition :: !(Surface.Position -> Core.Position)
  }

type TransliterateT = StateT TransState

-- | Run a computation inside the context of a block.
inBlock :: Monad m =>
           Surface.Position
        -- ^ The position of the block.
        -> TransliterateT m a
        -- ^ The computation to run.
        -> TransliterateT m a
        -- ^ The computation run in the context of the block.
inBlock blockpos action =
  let
    blockfunc posfunc Basic.Span { Basic.spanStart = start,
                                   Basic.spanEnd = end } =
      DWARF.Block { DWARF.blockCtx = posfunc blockpos,
                    DWARF.blockPos = DWARF.Span { DWARF.spanStart = start,
                                                  DWARF.spanEnd = end } }
    blockfunc posfunc Basic.Point { Basic.pointPos = pos } =
      DWARF.Block { DWARF.blockCtx = posfunc blockpos,
                    DWARF.blockPos = DWARF.Point { DWARF.pointPos = pos } }
    blockfunc posfunc Basic.File { Basic.fileName = fname } =
        DWARF.File { DWARF.fileName = fname }
    blockfunc posfunc Basic.Synthetic { Basic.synthDesc = desc } =
        DWARF.Synthetic { DWARF.synthDesc = desc }
    blockfunc posfunc Basic.CmdLine = DWARF.CmdLine
  in do
    state @ TransState { transPositionStack = stack,
                         transPosition = mkpos } <- get
    put state { transPosition = blockfunc mkpos,
                transPositionStack = mkpos : stack }
    res <- action
    put state
    return res

-- | Convert a 'BasicPosition' to a 'DWARFPosition'.
dwarfPos :: Monad m =>
            Surface.Position
         -- ^ The 'BasicPosition' to turn into a 'DWARFPosition'
         -> TransliterateT m Core.Position
dwarfPos pos =
  do
    state @ TransState { transPosition = mkpos } <- get
    return $! mkpos pos

genPattern :: Monad m =>
              Surface.Pattern (Surface.Exp Surface.Apply Surface.Ref)
           -> TransliterateT m (Core.Pattern Symbol)
genPattern Surface.Option { Surface.optionPats = pats,
                            Surface.optionPos = pos } =
  do
    corepats <- mapM genPattern pats
    corepos <- dwarfPos pos
    -- XXX Core doesn't presently have an Options pattern
    _
genPattern Surface.Deconstruct {
             Surface.deconstructPat = Surface.Split {
                                        Surface.splitFields = fields,
                                        Surface.splitStrict = strict
                                      },
             Surface.deconstructName = name,
             Surface.deconstructPos = pos
           } =
  do
    _
genPattern Surface.Deconstruct { Surface.deconstructPat = pat,
                                 Surface.deconstructName = name,
                                 Surface.deconstructPos = pos } = _
genPattern Surface.As { Surface.asName = sym, Surface.asPat = pat,
                        Surface.asPos = pos } =
  do
    corepat <- genPattern pat
    corepos <- dwarfPos pos
    return Core.As { Core.asName = sym, Core.asBind = corepat,
                     Core.asPos = corepos }
genPattern Surface.Name { Surface.nameSym = sym, Surface.namePos = pos } =
  do
    corepos <- dwarfPos pos
    return Core.Name { Core.nameSym = sym, Core.namePos = corepos }

-- | Conrvert a case to the core language.
genCase :: Monad m =>
           Surface.Case (Surface.Exp Surface.Apply Surface.Ref)
        -> TransliterateT m (Core.Case Symbol Symbol)
genCase Surface.Case { Surface.casePat = pat, Surface.caseBinds = binds,
                       Surface.caseBody = body, Surface.casePos = pos } =
  let
    -- | Set of symbols bound by the pattern.
    bindset :: HashSet Symbol
    bindset = HashSet.fromList binds

    -- | Abstraction function passed to abstract.
    absfunc :: Symbol -> Maybe Symbol
    absfunc sym
      | HashSet.member sym bindset = Just sym
      | otherwise = Nothing
  in do
    -- First convert everything
    corepos <- dwarfPos pos
    corepat <- genPattern pat
    corebody <- genIntroExp body
    -- Run abstract to build the proper 'Scope' for the body
    return Core.Case { Core.caseBody = abstract absfunc corebody,
                       Core.casePat = corepat, Core.casePos = corepos }

genIntroExp :: Monad m =>
               Surface.Exp Surface.Apply Surface.Ref
            -> TransliterateT m (Core.Intro Symbol Symbol)
genIntroExp Surface.Abs { Surface.absKind = kind,
                          Surface.absCases = cases,
                          Surface.absPos = pos } =
  do
    corepos <- dwarfPos pos
    corecases <- mapM genCase cases
    _
-- These all give rise to elimination terms
genIntroExp s @ Surface.Ascribe {} =
  do
    coreexp <- genElimExp s
    return Core.Elim { Core.elimTerm = coreexp }
genIntroExp s @ Surface.Call {} =
  do
    coreexp <- genElimExp s
    return Core.Elim { Core.elimTerm = coreexp }
genIntroExp s @ Surface.Sym {} =
  do
    coreexp <- genElimExp s
    return Core.Elim { Core.elimTerm = coreexp }
-- Just map bad terms to bad terms
genIntroExp Surface.Bad { Surface.badPos = pos } =
  do
    corepos <- dwarfPos pos
    return Core.BadIntro { Core.badIntroPos = corepos }

genElimExp :: Monad m =>
              Surface.Exp Surface.Apply Surface.Ref
           -> TransliterateT m (Core.Elim Symbol Symbol)
-- Convert these directly into elimination terms
genElimExp Surface.Ascribe { Surface.ascribeType = ty, Surface.ascribeVal = val,
                             Surface.ascribePos = pos } =
  do
    corepos <- dwarfPos pos
    corety <- genIntroExp ty
    coreterm <- genIntroExp val
    return Core.Typed { Core.typedTerm = coreterm, Core.typedType = corety,
                        Core.typedPos = corepos }
genElimExp Surface.Call {
             Surface.callInfo = Surface.Apply {
                                  Surface.applyFunc = func,
                                  Surface.applyArg = arg,
                                  Surface.applyPos = pos
                                }
           } =
  do
    corepos <- dwarfPos pos
    corefunc <- genElimExp func
    corearg <- genIntroExp arg
    return Core.Call { Core.callFunc = corefunc, Core.callArg = corearg,
                       Core.callPos = corepos }
genElimExp Surface.Sym {} = _
-- Just map bad terms to bad terms
genElimExp Surface.Bad { Surface.badPos = pos } =
  do
    corepos <- dwarfPos pos
    return Core.BadElim { Core.badElimPos = corepos }

-- XXX We need some way of representing components in the core language.

-- XXX Figure out what the top-level core language structure should be
{-
transliterate :: Monad m =>
                 Surface.Surface (Surface.Exp Surface.Apply Surface.Ref)
              -> m Core.Core
transliterate = _
-}
