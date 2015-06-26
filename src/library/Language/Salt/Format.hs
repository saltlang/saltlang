-- Copyright (c) 2015 Eric McCorkle.  All rights reserved.
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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- | Canonical formatting for the Salt language.  This module defines
-- functions for canonical pretty-printing of the Salt AST, as well
-- collections of 'Doc's representing Salt syntax fragments.
module Language.Salt.Format(
       -- * Low-Level Formatting Functions
       recordDoc,
       tupleDoc,
       listDoc,
       mapDoc,
       compoundApplyDoc
       ) where

import Text.Format

-- | Format a list of @(field, value)@ bindings representing a record
-- value.  There are three possible ways to do this:
--
-- > (field1 = value1, field2 = value2, ...)
-- >
-- > preceeding (field1 = value1,
-- >             field2 = value2,
-- >             ...)
-- >
-- > preceeding (
-- >     field1 = value1,
-- >     field2 = value2,
-- >     ...
-- >   )
recordDoc :: [(Doc, Doc)]
          -- ^ A list of @(field, value)@ bindings
          -> Doc
recordDoc binds =
  let
    softlines = map (\(field, val) -> field <+> equals </> nest 2 val) binds
    nosoftlines = map (\(field, val) -> field <+> equals <+> val) binds
    nobreaks = hsep (punctuate comma nosoftlines)
    breaks = vsep (punctuate comma softlines)
    breakopts = [ lparen <> align breaks <> rparen,
                  nest 2 (lparen <!> nest 2 breaks <!> rparen) ]
  in case flatten nobreaks of
    Just nolines -> choose (lparen <> nolines <> rparen : breakopts)
    Nothing -> choose breakopts

-- | Format a list of 'Doc's representing a tuple value.  There are
-- three possible ways to do this:
--
-- > (value1, value2, ...)
-- >
-- > preceeding (value1,
-- >             value2,
-- >             ...)
-- >
-- > preceeding (
-- >     value1,
-- >     value2,
-- >     ...
-- >   )
tupleDoc :: [Doc]
         -- ^ The fields of the tuple.
         -> Doc
tupleDoc fields =
  let
    nobreaks = hsep (punctuate comma fields)
    breaks = vsep (punctuate comma fields)
    breakopts = [ lparen <> align breaks <> rparen,
                  nest 2 (lparen <!> nest 2 breaks <!> rparen) ]
  in case flatten nobreaks of
    Just nolines -> choose (lparen <> nolines <> rparen : breakopts)
    Nothing -> choose breakopts

-- | Format a list of 'Doc's representing a list value.  There are
-- three possible ways to do this:
--
-- > [value1, value2, ...]
-- >
-- > preceeding [value1,
-- >             value2,
-- >             ...]
-- >
-- > preceeding [
-- >     value1,
-- >     value2,
-- >     ...
-- >   ]
listDoc :: [Doc]
         -- ^ The fields of the tuple.
         -> Doc
listDoc fields =
  let
    nobreaks = hsep (punctuate comma fields)
    breaks = vsep (punctuate comma fields)
    breakopts = [ lparen <> align breaks <> rparen,
                  nest 2 (lparen <!> nest 2 breaks <!> rparen) ]
  in case flatten nobreaks of
    Just nolines -> choose (lparen <> nolines <> rparen : breakopts)
    Nothing -> choose breakopts

-- | Format a map as a list of key/value pairs.
mapDoc :: [(Doc, Doc)]
       -- ^ The @(key, value)@ pairs for the map.
       -> Doc
mapDoc = listDoc . map (\(a, b) -> tupleDoc [a, b])

-- | Format a 'Doc' and a list of @(field, value)@ bindings
-- representing arguments.  There are three possible ways to do this:
--
-- > name (field1 = value1, field2 = value2, ...)
-- >
-- > name (field1 = value1,
-- >       field2 = value2,
-- >       ...)
-- >
-- > name (
-- >     field1 = value1,
-- >     field2 = value2
-- >     ...
-- >   )
compoundApplyDoc :: Doc
                 -- ^ The function or constructor.
                 -> [(Doc, Doc)]
                 -- ^ A list of @(field, value)@ bindings
                 -> Doc
compoundApplyDoc name = (name <+>) . recordDoc
