-- Copyright (c) 2014 Eric McCorkle.
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301 USA
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- | Abstract Syntax structure.  This is
module Language.Salt.Surface.Syntax(
       Defs(..),
       Scope(..),
       Content(..),
       Element(..),
       Compound(..),
       Pattern(..),
       Literal(..),
       Exp(..),
       Entry(..),
       Fields(..),
       Field(..),
       Case(..)
       ) where

import Data.Array
import Data.Position
import Data.Monoid
import Data.Symbol
import Data.Word
import Data.HashMap.Strict(HashMap)
import Language.Salt.Surface.Common

import qualified Data.HashMap.Strict as HashMap

-- | A set of definitions for a given symbol.  Represented as an array
-- of lists of 'Element's, indexed by 'Visibility', representing all
-- the definitions at that visibility.
newtype Defs = Defs { defs :: Array Visibility [Element] }

-- | A scope.  Consists of a map from symbols to definition sets
-- represented as 'Defs'.
data Scope =
  Scope {
    -- | All definitions in this scope
    scopeDefs :: !(HashMap Symbol Defs)
  }

-- | Content of a definition.  Used wherever we can see a definition
-- body, or else an expression.
data Content =
    -- | An actual definition body.
    Body !Scope
    -- | An expression.
  | Value !Exp

-- | Scope elements.  These represent declarations, imports, and truth
-- statements inside a scope.  Note: some of these can be built from
-- others.
data Element =
    -- | Value definitions.  These are declarations coupled with
    -- values.  These include function declarations.
    Def {
      -- | The value's initializer.
      defInit :: !(Maybe Exp),
      -- | The position in source from which this arises.
      defPos :: !Position
    }
    -- | Truths.  These are similar to declarations, except that they
    -- affect the proof environment.  These include theorems and
    -- invariants.
  | Truth {
      -- | The class of the truth.
      truthKind :: !TruthKind,
      -- | The truth proposition.
      truthContent :: !Exp,
      -- | The position in source from which this arises.
      truthPos :: !Position
    }
    -- | A proof.  This is just a code block with the name of the
    -- theorem being proven.
  | Proof {
      -- | The body of the proof.
      proofBody :: !Exp,
      -- | The position in source from which this arises.
      proofPos :: !Position
    }

-- | Compound expression elements.  These are either "ordinary"
-- expressions, or declarations.
data Compound =
    -- | An ordinary expression.
    Exp !Exp
    -- | A definition.
  | Element !Element

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
    -- expressions.
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
    -- | A sequence expressions.  The entire sequence represents a
    -- function call, possibly with inorder symbols.  This is
    -- re-parsed once the inorder symbols are known.
  | Seq {
      -- | The first expression.
      seqExps :: ![Exp],
      -- | The position in source from which this arises.
      seqPos :: !Position
    }
  | RecordType {
      recordTypeFields :: !Fields,
      recordTypePos :: !Position
    }
    -- | A record literal.  Can represent a record type, or a record value.
  | Record {
      -- | The fields in this record expression.
      recordFields :: !(HashMap Symbol Exp),
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
      projectName :: !Symbol,
      -- | The position in source from which this arises.
      projectPos :: !Position
    }
    -- | Reference to a name.  Note: this is all handled with the
    -- Bound framework.
  | Sym {
      -- | The name being referenced.
      symName :: !Symbol,
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
    -- | Builder.
  | Builder {
      -- | The type of entity the builder represents.
      builderKind :: !BuilderKind,
      -- | The declared supertypes for this builder entity.
      builderSuperTypes :: ![Exp],
      -- | The parameters of the builder entity.
      builderParams :: !Fields,
      -- | The entities declared by the builder.
      builderContent :: !Scope,
      -- | The position in source from which this arises.
      builderPos :: !Position
    }
    -- | Number literal.
  | Literal !Literal

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
    fieldsBindings :: !(HashMap Symbol Field),
    -- | A mapping from field positions to names.
    fieldsOrder :: !(Array Word Symbol)
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

instance Monoid Scope where
  mempty = Scope { scopeDefs = HashMap.empty }

  mappend Scope { scopeDefs = defs1 } Scope { scopeDefs = defs2 } =
    let
      combine Defs { defs = arr1 } Defs { defs = arr2 } =
        let
          (lo1, hi1) = bounds arr1
          (lo2, hi2) = bounds arr2
          lo = min lo1 lo2
          hi = max hi1 hi2

          getEntries idx
            | idx >= lo1 && idx <= hi1 && idx >= lo2 && idx <= hi2 =
              arr1 ! idx ++ arr2 ! idx
            | idx < lo1 && idx <= hi1 = arr2 ! idx
            | otherwise = arr1 ! idx

          values = map getEntries (enumFromTo lo hi)
        in
          Defs { defs = listArray (lo, hi) values }
    in
      Scope { scopeDefs = HashMap.unionWith combine defs1 defs2 }
