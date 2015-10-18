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

module Language.Salt.Surface.Bindings(
       Scope(..),
       Proof(..),
       Truth(..),
       Pattern(..),
       Exp(..),
       Entry(..),
       Fields(..),
       Field(..),
       Case(..)
       ) where

import Control.Monad.Positions
import Data.Array
import Data.HashMap.Strict(HashMap)
import Data.Symbol
import Data.Word
import Language.Salt.Surface.Common

-- | A single value binding.
data Bind =
    -- | A type-only binding
    Stub { stubType :: !Exp }
    -- | A typeless expression binding
  | Typeless { typelessValue :: !Exp }
    -- | A full binding, with a type and a value.
  | Full {
      -- | The type of the binding.
      fullType :: !Exp,
      -- | The value of the binding.
      fullValue :: !Exp
    }

-- | The set of value bindings for a given name.  Note that the set of
-- visible bindings depends on the visibility level at which you're
-- operating.
data Binds =
  Binds {
    -- | The definition context for this name.
    bindsContext :: !ContextKind,
    -- | An array of telescoping lists of 'Binding's accessible under
    -- this name, indexed by visibility and context kind.  For
    -- example, the list for Public includes the list for Protected,
    -- and so on.  Similarly, the list for Object includes the list
    -- for Local and Static.
    bindsViews :: !(Array Visibility [Bind])
  }

-- | Local definitions for a given scope.
data Defs =
  Defs {
    -- | All the builders defined in this scope.
    defsBuilders :: !(HashMap Symbol Builder),
    -- | The truth environment for this scope.  This contains all
    -- theorems, axioms, and invariants.
    defsTruths :: !(HashMap Symbol Truth),
    -- | Value bindings.
    defsVals :: !(HashMap Symbol Binds)
  }

data Scope =
  Scope {
    -- | Inherited scopes
    scopeInherits :: !HashMap ScopeID Scope,
    -- | Direct definitions.
    scopeDefs :: !Defs
  }

-- | A builder definition.
data Builder =
  Builder {
    -- | The type of entity the builder represents.
    builderKind :: !BuilderKind,
    -- | The visibility of this definition.
    builderVisibility :: !Visibility,
    -- | The parameters of the builder entity.
    builderParams :: !Fields,
    -- | The declared supertypes for this builder entity.
    builderSuperTypes :: ![Exp],
    -- | The entities declared by the builder.
    builderContent :: !Exp,
    -- | The position in source from which this arises.
    builderPos :: !Position
  }

data Proof =
  -- | A proof.  This is just a code block with the name of the
  -- theorem being proven.
  Proof {
    -- | The body of the proof.
    proofBody :: !Exp,
    -- | The position in source from which this arises.
    proofPos :: !Position
  }

-- | Truths.  These are similar to declarations, except that they
-- affect the proof environment.  These include theorems and
-- invariants.
data Truth scope =
  Truth {
    -- | The class of the truth.
    truthKind :: !TruthKind,
    -- | The visibility of the truth.
    truthVisibility :: !Visibility,
    -- | The truth proposition.
    truthContent :: !Exp,
    -- | A proof (may or may not be supplied).
    truthProof :: !(Maybe Exp),
    -- | The position in source from which this arises.
    truthPos :: !Position
  }

-- | A pattern, for pattern match expressions.
data Pattern =
    Option {
      -- | The option patterns
      optionPats :: ![Pattern],
      -- | The position in source from which this arises.
      optionPos :: !Position
    }
    -- | A deconstructor pattern.  Mirrors a call expression.
  | Deconstruct {
      -- | The name of the constructor.
      deconstructName :: !Symbol,
      -- | The arguments to the constructor.
      deconstructPat :: !Pattern,
      -- | The position in source from which this arises.
      deconstructPos :: !Position
    }
    -- | A projection.  Mirrors a record expression.
  | Split {
      -- | The fields being projected.
      splitFields :: !(HashMap Symbol Entry),
      -- | Whether or not the binding is strict (ie. it omits some fields)
      splitStrict :: !Bool,
      -- | The position in source from which this arises.
      splitPos :: !Position
    }
    -- | A typed pattern.  Fixes the type of a pattern.
  | Typed {
      -- | The pattern whose type is being fixed.
      typedPat :: !Pattern,
      -- | The type to which the pattern is fixed.
      typedType :: !Exp,
      -- | The position in source from which this arises.
      typedPos :: !Position
    }
    -- | An as pattern.  Captures part of a pattern as a name, but
    -- continues to match.
    --
    -- Corresponds to the syntax
    -- > name as (pattern)
  | As {
      -- | The name to bind.
      asName :: !Symbol,
      -- | The sub-pattern to match.
      asPat :: !Pattern,
      -- | The position in source from which this arises.
      asPos :: !Position
    }
    -- | A name binding.  Binds the given name to the contents.
  | Name {
      -- | The name to bind.
      nameSym :: !Symbol,
      -- | The position in source from which this arises.
      namePos :: !Position
    }
  | Exact !Literal

-- | Expressions.  These represent computed values of any type.
data Exp =
    -- | An expression that may contain declarations as well as
    -- expressions.  This structure defines a dynamic scope.  Syntax
    -- directives and proofs are moved out-of-line, while truths,
    -- builders, and regular definitions remain in-line.
    Compound {
      -- | The body of the compound expression.
      compoundBody :: ![Compound],
      -- | The position in source from which this arises.
      compoundPos :: !Position
    }
    -- | An abstraction.  Generally represents anything with
    -- parameters and cases.  Specifically, represents lambdas,
    -- foralls, and exists.
  | Abs {
      -- | The kind of the abstraction.
      absKind :: !AbstractionKind,
      -- | The cases for the abstraction.
      absCases :: ![Case],
      -- | The position in source from which this arises.
      absPos :: !Position
    }
    -- | Match statement.  Given a set of (possibly typed) patterns,
    -- find the meet of all types above the argument's type in the
    -- cases, then apply the first pattern for the meet type that
    -- matches the argument.
  | Match {
      -- | The argument to the match statement.
      matchVal :: !Exp,
      -- | The cases, in order.
      matchCases :: ![Case],
      -- | The position in source from which this arises.
      matchPos :: !Position
    }
    -- | Ascribe expression.  Fixes the type of a given expression.
  | Ascribe {
      -- | The expression whose type is being set.
      ascribeVal :: !Exp,
      -- | The type.
      ascribeType :: !Exp,
      -- | The position in source from which this arises.
      ascribePos :: !Position
    }
    -- | An apply expression.  Represents a function call.
  | Apply {
      -- | The function being applied.
      applyFunc :: !Exp,
      -- | The argument.  Note that we turn a list of arguments into a tuple.
      applyArgs :: !Exp,
      -- | The position in source from which this arises.
      applyPos :: !Position
    }
  | RecordType {
      recordTypeFields :: !Fields,
      recordTypePos :: !Position
    }
    -- | A record literal.  Can represent a record type, or a record value.
  | Record {
      -- | The fields in this record expression.
      recordFields :: !(HashMap FieldName Exp),
      -- | The position in source from which this arises.
      recordPos :: !Position
    }
    -- | A tuple literal.
  | Tuple {
      -- | The fields in this tuple expression.
      tupleFields :: ![Exp],
      -- | The position in source from which this arises.
      tuplePos :: !Position
    }
    -- | A field expression.  Accesses the given field in a record.
    -- Note that for non-static functions, this implies an extra
    -- argument.
  | Project {
      -- | The inner expression
      projectVal :: !Exp,
      -- | The name of the field being accessed.
      projectFields :: ![FieldName],
      -- | The position in source from which this arises.
      projectPos :: !Position
    }
    -- | Reference to a name.  Note: this is all handled with the
    -- Bound framework.
  | Sym {
      -- | The name being referenced.
      symName :: !Symbol,
      -- | The scope in which this symbol is defined.
      symScope :: !ScopeID,
      -- | The position in source from which this arises.
      symPos :: !Position
    }
    -- | A with expression.  Represents currying.
  | With {
      -- | The value to which some arguments are being applied.
      withVal :: !Exp,
      -- | The argument(s) to apply
      withArgs :: !Exp,
      -- | The position in source from which this arises.
      withPos :: !Position
    }
    -- | Where expression.  Constructs refinement types.
  | Where {
      -- | The value to which some arguments are being applied.
      whereVal :: !Exp,
      -- | The argument(s) to apply
      whereProp :: !Exp,
      -- | The position in source from which this arises.
      wherePos :: !Position
    }
    -- | Builder literal.
  | Anon {
      -- | The type of entity the builder represents.
      anonKind :: !BuilderKind,
      -- | The declared supertypes for this builder entity.
      anonSuperTypes :: ![Exp],
      -- | The parameters of the builder entity.
      anonParams :: !Fields,
      -- | The entities declared by the builder.
      anonContent :: !Scope,
      -- | The position in source from which this arises.
      anonPos :: !Position
    }
    -- | Number literal.
  | Literal { literalVal :: !Literal }
    -- | A placeholder for a bad expression.
  | BadExp { badExpPos :: !Position }

-- | An entry in a record field or call argument list.
data Entry =
  -- | A named field.  This sets a specific field or argument to a
  -- certain value.
  Entry {
    -- | The value to which the field or argument is set.
    entryPat :: !Pattern,
    -- | The position in source from which this arises.
    entryPos :: !Position
   }

-- | Representation of fields.  This contains information for
-- interpreting record as well as tuple values.
data Fields =
  Fields {
    -- | Named bindings.
    fieldsBindings :: !(HashMap FieldName Field),
    -- | A mapping from field positions to names.
    fieldsOrder :: !(Array Word FieldName)
  }

-- | A field.
data Field =
  Field {
    -- | The value assigned to the bound name.
    fieldVal :: !Exp,
    -- | The position in source from which this arises.
    fieldPos :: !Position
  }

-- | A case in a match statement or a function definition.
data Case =
  Case {
    -- | The pattern to match for this case.
    casePat :: !Pattern,
    -- | The expression to execute for this case.
    caseBody :: !Exp,
    -- | The position in source from which this arises.
    casePos :: !Position
  }
