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
       compoundApplyDoc,
       blockDoc,
       stmsDoc,
       casesDoc,
       nestLevel
       ) where

import Text.Format

nestLevel :: Int
nestLevel = 2

-- | Format a list of @(field, value)@ bindings representing a record
-- value.  There are two possible ways to do this:
--
-- > (field1 = value1, field2 = value2, ...)
--
-- > (field1 = value1,
-- >  field2 = value2,
-- >  ...)
recordDoc :: [(Doc, Doc)]
          -- ^ A list of @(field, value)@ bindings
          -> Doc
recordDoc binds =
  let
    softlines = map (\(field, val) -> field <+> equals </>
                                      nest nestLevel val) binds
    nosoftlines = map (\(field, val) -> field <+> equals <+> val) binds
    nobreaks = hsep (punctuate comma nosoftlines)
    alignbreaks = parens (align (vsep (punctuate comma softlines)))
    nestbreaks = lparen <!> nest 2 (vsep (punctuate comma softlines)) <!> rparen
  in case flatten nobreaks of
    Just nolines -> choose [parens nolines, alignbreaks, nestbreaks]
    Nothing -> choose [ alignbreaks, nestbreaks ]

-- | Format a list of 'Doc's representing a tuple value.  There are
-- three possible ways to do this:
--
-- > (value1, value2, ...)
--
-- > (value1,
-- >  value2,
-- >  ...)
tupleDoc :: [Doc]
         -- ^ The fields of the tuple.
         -> Doc
tupleDoc fields =
  let
    nobreaks = hsep (punctuate comma fields)
    alignbreaks = parens (align (vsep (punctuate comma fields)))
    nestbreaks = lparen <!> nest 2 (vsep (punctuate comma fields)) <!> rparen
  in case flatten nobreaks of
    Just nolines -> choose [parens nolines, alignbreaks, nestbreaks]
    Nothing -> choose [ alignbreaks, nestbreaks ]

-- | Format a list of 'Doc's representing a list value.  There are
-- three possible ways to do this:
--
-- > [value1, value2, ...]
--
-- > preceeding [value1,
-- >             value2,
-- >             ...]
--
-- > preceeding [
-- >     value1,
-- >     value2,
-- >     ...
-- >   ]
listDoc :: [Doc]
         -- ^ The fields of the list.
         -> Doc
listDoc fields =
  let
    nobreaks = hsep (punctuate comma fields)
    alignbreaks = brackets (align (vsep (punctuate comma fields)))
    nestbreaks = lbrack <!> nest 2 (vsep (punctuate comma fields)) <!> rbrack
  in case flatten nobreaks of
    Just nolines -> choose [brackets nolines, alignbreaks, nestbreaks]
    Nothing -> choose [ alignbreaks, nestbreaks ]

-- | Format a map as a list of key/value pairs.
mapDoc :: [(Doc, Doc)]
       -- ^ The @(key, value)@ pairs for the map.
       -> Doc
mapDoc = listDoc . map (\(a, b) -> tupleDoc [a, b])

-- | Format a 'Doc' and a list of @(field, value)@ bindings
-- representing arguments.  There are three possible ways to do this:
--
-- > name (field1 = value1, field2 = value2, ...)
--
-- > name (field1 = value1,
-- >       field2 = value2,
-- >       ...)
--
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

-- | Format a list of 'Doc's representing statements in a block.
-- There are two possible ways to do this:
--
-- > { value1; value2; ... }
--
-- > preceeding {
-- >     value1;
-- >     value2;
-- >     ...
-- >   }
blockDoc :: [Doc]
         -- ^ The content of the block
         -> Doc
blockDoc stms =
  let
    nobreaks = hsep (punctuate semi stms)
    breaks = vsep (punctuate semi stms)
    breakopts = [ nest nestLevel (lbrace <!> nest nestLevel breaks <!> rbrace) ]
  in case flatten nobreaks of
    Just nolines -> choose (lbrace <+> nolines <+> rbrace : breakopts)
    Nothing -> choose breakopts

-- | Format a list of 'Doc's representing statements.
-- There are two possible ways to do this:
--
-- > value1; value2; ...
--
-- > value1;
-- > value2;
-- > ...
stmsDoc :: [Doc]
         -- ^ The content of the block
         -> Doc
stmsDoc stms =
  let
    nobreaks = hsep (punctuate semi stms)
    breaks = vsep (punctuate semi stms)
  in case flatten nobreaks of
    Just nolines -> choose [ nolines, breaks ]
    Nothing -> breaks

-- | Format a 'Doc' and a list of bindings representing cases in a
-- pattern group.  There are three possible ways to do this:
--
-- > preceeding case1 | case2 | ...
--
-- > preceeding case1
-- >          | case2
-- >          | ...
--
-- > preceeding
-- >     case1
-- >   | case2
-- >   | ...
casesDoc :: [Doc]
         -- ^ The cases.
         -> Doc
casesDoc cases =
  let
    nobreaks = hsep (punctuate (string " | ") cases)
    breaks = vsep (punctuate (string "| ") cases)
    breakopts = [ alignOffset (-1) breaks,
                  nest nestLevel (hardline <> string "  " <> breaks) ]
  in case flatten nobreaks of
    Just nolines -> choose (parens nolines : breakopts)
    Nothing -> choose breakopts
