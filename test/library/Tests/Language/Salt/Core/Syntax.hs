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
{-# LANGUAGE OverloadedStrings #-}

module Tests.Language.Salt.Core.Syntax(tests) where

import Bound
import Data.Position.DWARFPosition
import Language.Salt.Core.Syntax
import Test.HUnitPlus.Base

import qualified Data.HashMap.Strict as HashMap

elimSubst :: [Test]
elimSubst =
  let
    unittype :: Intro Int Int
    unittype = RecordType { recTypeBody = [], recTypePos = Synthetic "0" }

    typetype :: Elim Int Int
    typetype = Var { varSym = -1, varPos = Synthetic "1" }

    unitval :: Intro Int Int
    unitval = Record { recFields = HashMap.empty, recPos = Synthetic "2" }

    substvar :: Elim Int Int
    substvar = Var { varSym = 0, varPos = Synthetic "3" }

    substvar2 :: Elim Int Int
    substvar2 = Var { varSym = 2, varPos = Synthetic "4" }

    substunitcall :: Elim Int Int -> Elim Int Int
    substunitcall func = Call { callArgs = HashMap.singleton 0 unitval,
                                callFunc = func, callPos = Synthetic "5" }

    substfuncty :: Intro Int Int -> Elim Int Int
    substfuncty arg = FuncType { }

    substcall :: Elim Int Int -> Intro Int Int -> Elim Int Int
    substcall func arg = Call { callArgs = HashMap.singleton 0 arg,
                                callFunc = func, callPos = Synthetic "6" }

    valvar :: Elim Int Int
    valvar = Var { varSym = 1, varPos = Synthetic "7" }

    substtyped :: Intro Int Int -> Intro Int Int -> Elim Int Int
    substtyped term ty = Typed { typedTerm = term, typedType = ty,
                                 typedPos = Synthetic "8" }

  in [ "var___var" ~: substitute 0 valvar substvar @?= valvar,
       "var___var2" ~: substitute 0 valvar substvar2 @?= substvar2,
       "unitcall___var" ~:
         substitute 0 valvar (substunitcall substvar) @?=
           (substunitcall valvar),
       "unitcall___var2" ~:
         substitute 0 valvar (substunitcall substvar2) @?=
           (substunitcall substvar2),
       "call___var_var" ~:
         substitute 0 valvar (substcall substvar (Elim substvar)) @?=
           (substcall valvar (Elim valvar)),
       "call___var2_var" ~:
         substitute 0 valvar (substcall substvar2 (Elim substvar)) @?=
           (substcall substvar2 (Elim valvar)),
       "call___var_var2" ~:
         substitute 0 valvar (substcall substvar (Elim substvar2)) @?=
           (substcall valvar (Elim substvar2)),
       "call___var_var2" ~:
         substitute 0 valvar (substcall substvar2 (Elim substvar2)) @?=
           (substcall substvar2 (Elim substvar2)),
       "typed___unit_unit" ~:
         substitute 0 valvar (substtyped unitval unittype) @?=
           (substtyped unitval unittype),
       "typed___var_unit" ~:
         substitute 0 valvar (substtyped (Elim substvar) unittype) @?=
           (substtyped (Elim valvar) unittype),
       "typed___var_var" ~:
         substitute 0 valvar (substtyped (Elim substvar) (Elim substvar)) @?=
           (substtyped (Elim valvar) (Elim valvar)),
       "typed___var2_var" ~:
         substitute 0 valvar (substtyped (Elim substvar2) (Elim substvar)) @?=
           (substtyped (Elim substvar2) (Elim valvar)),
       "typed___var_var2" ~:
         substitute 0 valvar (substtyped (Elim substvar) (Elim substvar2)) @?=
           (substtyped (Elim valvar) (Elim substvar2)),
       "typed___var2_var2" ~:
         substitute 0 valvar (substtyped (Elim substvar2) (Elim substvar2)) @?=
           (substtyped (Elim substvar2) (Elim substvar2))
     ]

testlist :: [Test]
testlist = [ "elim_subst" ~: elimSubst ]

tests :: Test
tests = "Syntax" ~: testlist
