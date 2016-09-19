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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Language.Salt.Surface.Scopes(
       Scope(..),
       Proof(..),
       Truth(..),
       Pattern(..),
       Exp(..),
       Entry(..),
       Fields(..),
       Field(..),
       Case(..),
       Scopes(..)
       ) where

import Data.Array
import Data.Position.DWARFPosition
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.List
import Data.Symbol
import Data.Word
import Language.Salt.Surface.Common
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)

import qualified Data.HashMap.Strict as HashMap

-- | A reference to a definition in a scope.
data Ref =
  Ref {
    -- | The symbol in the scope.
    refSymbol :: !Symbol,
    -- | The scope ID.
    refScopeID :: !ScopeID
  }
  deriving (Ord, Eq)

-- | A single value binding.
data Bind refty =
    -- | A type-only binding
    Stub { stubType :: !(Exp refty) }
    -- | A typeless expression binding
  | Typeless { typelessVal :: !(Exp refty) }
    -- | A full binding, with a type and a value.
  | Full {
      -- | The type of the binding.
      fullType :: !(Exp refty),
      -- | The value of the binding.
      fullVal :: !(Exp refty)
    }

-- | The set of value bindings for a given name.  Note that the set of
-- visible bindings depends on the visibility level at which you're
-- operating.
data Binds refty =
  Binds {
    -- | The definition context for this name.
    bindsContext :: !ContextKind,
    -- | An array of telescoping lists of 'Binding's accessible under
    -- this name, indexed by visibility and context kind.  For
    -- example, the list for Public includes the list for Protected,
    -- and so on.  Similarly, the list for Object includes the list
    -- for Local and Static.
    bindsViews :: !(Array Visibility [Bind refty])
  }

-- | Local definitions for a given scope.
data Defs refty =
  Defs {
    -- | All the builders defined in this scope.
    defsBuilders :: !(HashMap Symbol (Builder refty)),
    -- | The truth environment for this scope.  This contains all
    -- theorems, axioms, and invariants.
    defsTruths :: !(HashMap Symbol (Truth refty)),
    -- | Value bindings.
    defsVals :: !(HashMap Symbol (Binds refty))
  }

data Scope refty =
  Scope {
    -- | Inherited scopes
    scopeInherits :: !(HashMap ScopeID (Scope refty)),
    -- | Direct definitions.
    scopeDefs :: !(Defs refty)
  }

-- | A builder definition.
data Builder refty =
  Builder {
    -- | The type of entity the builder represents.
    builderKind :: !BuilderKind,
    -- | The visibility of this definition.
    builderVisibility :: !Visibility,
    -- | The parameters of the builder entity.
    builderParams :: !(Fields refty),
    -- | The declared supertypes for this builder entity.
    builderSuperTypes :: ![Exp refty],
    -- | The entities declared by the builder.
    builderContent :: !(Exp refty),
    -- | The position in source from which this arises.
    builderPos :: !(DWARFPosition Ref Ref)
  }

data Proof refty =
  -- | A proof.  This is just a code block with the name of the
  -- theorem being proven.
  Proof {
    -- | The body of the proof.
    proofBody :: !(Exp refty),
    -- | The position in source from which this arises.
    proofPos :: !(DWARFPosition Ref Ref)
  }

-- | Truths.  These are similar to declarations, except that they
-- affect the proof environment.  These include theorems and
-- invariants.
data Truth refty =
  Truth {
    -- | The class of the truth.
    truthKind :: !TruthKind,
    -- | The visibility of the truth.
    truthVisibility :: !Visibility,
    -- | The truth proposition.
    truthContent :: !(Exp refty),
    -- | A proof (may or may not be supplied).
    truthProof :: !(Maybe (Exp refty)),
    -- | The position in source from which this arises.
    truthPos :: !(DWARFPosition Ref Ref)
  }

-- | A pattern, for pattern match expressions.
data Pattern refty =
    Option {
      -- | The option patterns
      optionPats :: ![Pattern refty],
      -- | The position in source from which this arises.
      optionPos :: !(DWARFPosition Ref Ref)
    }
    -- | A deconstructor pattern.  Mirrors a call expression.
  | Deconstruct {
      -- | The name of the constructor.
      deconstructName :: !Symbol,
      -- | The arguments to the constructor.
      deconstructPat :: !(Pattern refty),
      -- | The position in source from which this arises.
      deconstructPos :: !(DWARFPosition Ref Ref)
    }
    -- | A projection.  Mirrors a record expression.
  | Split {
      -- | The fields being projected.
      splitFields :: !(HashMap Symbol (Entry refty)),
      -- | Whether or not the binding is strict (ie. it omits some fields)
      splitStrict :: !Bool,
      -- | The position in source from which this arises.
      splitPos :: !(DWARFPosition Ref Ref)
    }
    -- | A typed pattern.  Fixes the type of a pattern.
  | Typed {
      -- | The pattern whose type is being fixed.
      typedPat :: !(Pattern refty),
      -- | The type to which the pattern is fixed.
      typedType :: !(Exp refty),
      -- | The position in source from which this arises.
      typedPos :: !(DWARFPosition Ref Ref)
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
      asPat :: !(Pattern refty),
      -- | The position in source from which this arises.
      asPos :: !(DWARFPosition Ref Ref)
    }
    -- | A name binding.  Binds the given name to the contents.
  | Name {
      -- | The name to bind.
      nameSym :: !Symbol,
      -- | The position in source from which this arises.
      namePos :: !(DWARFPosition Ref Ref)
    }
  | Exact { exactLit :: !Literal }

-- | Expressions.  These represent computed values of any type.
data Exp refty =
    -- | An expression that may contain declarations as well as
    -- expressions.  This structure defines a dynamic scope.  Syntax
    -- directives and proofs are moved out-of-line, while truths,
    -- builders, and regular definitions remain in-line.
    Compound {
      -- | The body of the compound expression.
--      compoundBody :: ![Compound refty],
      -- | The position in source from which this arises.
      compoundPos :: !(DWARFPosition Ref Ref)
    }
    -- | An abstraction.  Generally represents anything with
    -- parameters and cases.  Specifically, represents lambdas,
    -- foralls, and exists.
  | Abs {
      -- | The kind of the abstraction.
      absKind :: !AbstractionKind,
      -- | The cases for the abstraction.
      absCases :: ![Case refty],
      -- | The position in source from which this arises.
      absPos :: !(DWARFPosition Ref Ref)
    }
    -- | Match statement.  Given a set of (possibly typed) patterns,
    -- find the meet of all types above the argument's type in the
    -- cases, then apply the first pattern for the meet type that
    -- matches the argument.
  | Match {
      -- | The argument to the match statement.
      matchVal :: !(Exp refty),
      -- | The cases, in order.
      matchCases :: ![Case refty],
      -- | The position in source from which this arises.
      matchPos :: !(DWARFPosition Ref Ref)
    }
    -- | Ascribe expression.  Fixes the type of a given expression.
  | Ascribe {
      -- | The expression whose type is being set.
      ascribeVal :: !(Exp refty),
      -- | The type.
      ascribeType :: !(Exp refty),
      -- | The position in source from which this arises.
      ascribePos :: !(DWARFPosition Ref Ref)
    }
    -- | An apply expression.  Represents a function call.
  | Apply {
      -- | The function being applied.
      applyFunc :: !(Exp refty),
      -- | The argument.  Note that we turn a list of arguments into a tuple.
      applyArgs :: !(Exp refty),
      -- | The position in source from which this arises.
      applyPos :: !(DWARFPosition Ref Ref)
    }
  | RecordType {
      recordTypeFields :: !(Fields refty),
      recordTypePos :: !(DWARFPosition Ref Ref)
    }
    -- | A record literal.  Can represent a record type, or a record value.
  | Record {
      -- | The fields in this record expression.
      recordFields :: !(HashMap FieldName (Exp refty)),
      -- | The position in source from which this arises.
      recordPos :: !(DWARFPosition Ref Ref)
    }
    -- | A tuple literal.
  | Tuple {
      -- | The fields in this tuple expression.
      tupleFields :: ![Exp refty],
      -- | The position in source from which this arises.
      tuplePos :: !(DWARFPosition Ref Ref)
    }
    -- | A field expression.  Accesses the given field in a record.
    -- Note that for non-static functions, this implies an extra
    -- argument.
  | Project {
      -- | The inner expression
      projectVal :: !(Exp refty),
      -- | The name of the field being accessed.
      projectFields :: ![FieldName],
      -- | The position in source from which this arises.
      projectPos :: !(DWARFPosition Ref Ref)
    }
    -- | Reference to a name.  Note: this is all handled with the
    -- Bound framework.
  | Sym {
      -- | The symbol reference
      symRef :: !refty,
      -- | The position in source from which this arises.
      symPos :: !(DWARFPosition Ref Ref)
    }
    -- | A with expression.  Represents currying.
  | With {
      -- | The value to which some arguments are being applied.
      withVal :: !(Exp refty),
      -- | The argument(s) to apply
      withArgs :: !(Exp refty),
      -- | The position in source from which this arises.
      withPos :: !(DWARFPosition Ref Ref)
    }
    -- | Where expression.  Constructs refinement types.
  | Where {
      -- | The value to which some arguments are being applied.
      whereVal :: !(Exp refty),
      -- | The argument(s) to apply
      whereProp :: !(Exp refty),
      -- | The position in source from which this arises.
      wherePos :: !(DWARFPosition Ref Ref)
    }
    -- | Builder literal.
  | Anon {
      -- | The type of entity the builder represents.
      anonKind :: !BuilderKind,
      -- | The declared supertypes for this builder entity.
      anonSuperTypes :: ![Exp refty],
      -- | The parameters of the builder entity.
      anonParams :: !(Fields refty),
      -- | The entities declared by the builder.
      anonContent :: !(Scope refty),
      -- | The position in source from which this arises.
      anonPos :: !(DWARFPosition Ref Ref)
    }
    -- | Number literal.
  | Literal { literalVal :: !Literal }
    -- | A placeholder for a bad expression.
  | BadExp { badExpPos :: !(DWARFPosition Ref Ref) }

-- | An entry in a record field or call argument list.
data Entry refty =
  -- | A named field.  This sets a specific field or argument to a
  -- certain value.
  Entry {
    -- | The value to which the field or argument is set.
    entryPat :: !(Pattern refty),
    -- | The position in source from which this arises.
    entryPos :: !(DWARFPosition Ref Ref)
   }

-- | Representation of fields.  This contains information for
-- interpreting record as well as tuple values.
data Fields refty =
  Fields {
    -- | Named bindings.
    fieldsBindings :: !(HashMap FieldName (Field refty)),
    -- | A mapping from field positions to names.
    fieldsOrder :: !(Array Word FieldName)
  }

-- | A field.
data Field refty =
  Field {
    -- | The value assigned to the bound name.
    fieldVal :: !(Exp refty),
    -- | The position in source from which this arises.
    fieldPos :: !(DWARFPosition Ref Ref)
  }

-- | A case in a match statement or a function definition.
data Case refty =
  Case {
    -- | The pattern to match for this case.
    casePat :: !(Pattern refty),
    -- | The expression to execute for this case.
    caseBody :: !(Exp refty),
    -- | The position in source from which this arises.
    casePos :: !(DWARFPosition Ref Ref)
  }

-- | Top-level construct, providing all scopes.
data Scopes refty =
  Scopes {
    -- | Array of all scopes.
    scopesArr :: !(Array ScopeID (Scope refty))
  }

instance Eq refty => Eq (Bind refty) where
  Stub { stubType = ty1 } == Stub { stubType = ty2 } = ty1 == ty2
  Typeless { typelessVal = val1 } == Typeless { typelessVal = val2 } =
    val1 == val2
  Full { fullType = ty1, fullVal = val1 } ==
    Full { fullType = ty2, fullVal = val2 } =
      ty1 == ty2 && val1 == val2
  _ == _ = False

instance Eq refty => Eq (Binds refty) where
  Binds { bindsContext = ctx1, bindsViews = views1 } ==
    Binds { bindsContext = ctx2, bindsViews = views2 } =
      ctx1 == ctx2 && views1 == views2

instance Eq refty => Eq (Defs refty) where
  Defs { defsBuilders = builders1, defsTruths = truths1, defsVals = vals1 } ==
    Defs { defsBuilders = builders2, defsTruths = truths2, defsVals = vals2 } =
      builders1 == builders2 && truths1 == truths2 && vals1 == vals2

instance Eq refty => Eq (Scope refty) where
  Scope { scopeInherits = inherits1, scopeDefs = defs1 } ==
    Scope { scopeInherits = inherits2, scopeDefs = defs2 } =
      inherits1 == inherits2 && defs1 == defs2

instance Eq refty => Eq (Builder refty) where
  Builder { builderKind = kind1, builderVisibility = vis1,
            builderParams = params1, builderSuperTypes = supers1,
            builderContent = content1 } ==
    Builder { builderKind = kind2, builderVisibility = vis2,
              builderParams = params2, builderSuperTypes = supers2,
              builderContent = content2 } =
      kind1 == kind2 && vis1 == vis2 && params1 == params2 &&
      supers1 == supers2 && content1 == content2

instance Eq refty => Eq (Proof refty) where
  Proof { proofBody = body1 } == Proof { proofBody = body2 } = body1 == body2

instance Eq refty => Eq (Truth refty) where
  Truth { truthKind = kind1, truthVisibility = vis1,
          truthContent = content1, truthProof = proof1 } ==
    Truth { truthKind = kind2, truthVisibility = vis2,
            truthContent = content2, truthProof = proof2 } =
      kind1 == kind2 && vis1 == vis2 &&
      proof1 == proof2 && content1 == content2

instance Eq refty => Eq (Pattern refty) where
  Option { optionPats = pats1 } == Option { optionPats = pats2 } =
    pats1 == pats2
  Deconstruct { deconstructName = name1, deconstructPat = pat1 } ==
    Deconstruct { deconstructName = name2, deconstructPat = pat2 } =
      name1 == name2 && pat1 == pat2
  Split { splitFields = fields1, splitStrict = strict1 } ==
    Split { splitFields = fields2, splitStrict = strict2 } =
      strict1 == strict2 && fields1 == fields2
  Typed { typedPat = pat1, typedType = ty1 } ==
    Typed { typedPat = pat2, typedType = ty2 } =
      pat1 == pat2 && ty1 == ty2
  As { asName = name1, asPat = pat1 } == As { asName = name2, asPat = pat2 } =
    name1 == name2 && pat1 == pat2
  Name { nameSym = sym1 } == Name { nameSym = sym2 } = sym1 == sym2
  Exact { exactLit = lit1 } == Exact { exactLit = lit2 } = lit1 == lit2
  _ == _ = False

instance Eq refty => Eq (Exp refty) where
  Compound {} == Compound {} = _
  Abs { absKind = kind1, absCases = cases1 } ==
    Abs { absKind = kind2, absCases = cases2 } =
      kind1 == kind2 && cases1 == cases2
  Match { matchVal = val1, matchCases = cases1 } ==
    Match { matchVal = val2, matchCases = cases2 } =
      val1 == val2 && cases1 == cases2
  Ascribe { ascribeVal = val1, ascribeType = type1 } ==
    Ascribe { ascribeVal = val2, ascribeType = type2 } =
      val1 == val2 && type1 == type2
  Apply { applyFunc = func1, applyArgs = args1 } ==
    Apply { applyFunc = func2, applyArgs = args2 } =
      func1 == func2 && args1 == args2
  RecordType { recordTypeFields = fields1 } ==
    RecordType { recordTypeFields = fields2 } =
      fields1 == fields2
  Record { recordFields = fields1 } == Record { recordFields = fields2 } =
    fields1 == fields2
  Tuple { tupleFields = fields1 } == Tuple { tupleFields = fields2 } =
    fields1 == fields2
  Project { projectVal = val1, projectFields = fields1 } ==
    Project { projectVal = val2, projectFields = fields2 } =
      fields1 == fields2 && val1 == val2
  Sym { symRef = ref1 } == Sym { symRef = ref2 } = ref1 == ref2
  With { withVal = val1, withArgs = args1 } ==
    With { withVal = val2, withArgs = args2 } =
      val1 == val2 && args1 == args2
  Where { whereVal = val1, whereProp = prop1 } ==
    Where { whereVal = val2, whereProp = prop2 } =
      val1 == val2 && prop1 == prop2
  Anon { anonKind = kind1, anonSuperTypes = supers1,
         anonParams = params1, anonContent = content1 } ==
    Anon { anonKind = kind2, anonSuperTypes = supers2,
           anonParams = params2, anonContent = content2 } =
      kind1 == kind2 && supers1 == supers2 &&
      params1 == params2 && content1 == content2
  Literal { literalVal = val1 } == Literal { literalVal = val2 } = val1 == val2
  BadExp {} == BadExp {} = True
  _ == _ = False

instance Eq refty => Eq (Entry refty) where
  Entry { entryPat = pat1 } == Entry { entryPat = pat2 } = pat1 == pat2

instance Eq refty => Eq (Fields refty) where
  Fields { fieldsBindings = binds1, fieldsOrder = order1 } ==
    Fields { fieldsBindings = binds2, fieldsOrder = order2 } =
      assocs order1 == assocs order2 && binds1 == binds2

instance Eq refty => Eq (Field refty) where
  Field { fieldVal = val1 } == Field { fieldVal = val2 } = val1 == val2

instance Eq refty => Eq (Case refty) where
  Case { casePat = pat1, caseBody = body1 } ==
    Case { casePat = pat2, caseBody = body2 } =
      pat1 == pat2 && body1 == body2

instance Eq refty => Eq (Scopes refty) where
  Scopes { scopesArr = arr1 } == Scopes { scopesArr = arr2 } =
    assocs arr1 == assocs arr2

instance Ord refty => Ord (Bind refty) where
  compare Stub { stubType = ty1 } Stub { stubType = ty2 } = compare ty1 ty2
  compare Stub {} _ = LT
  compare _ Stub {} = GT
  compare Typeless { typelessVal = val1 } Typeless { typelessVal = val2 } =
    compare val1 val2
  compare Typeless {} _ = LT
  compare _ Typeless {} = GT
  compare Full { fullType = ty1, fullVal = val1 }
          Full { fullType = ty2, fullVal = val2 } =
    case compare ty1 ty2 of
      EQ -> compare val1 val2
      out -> out

instance Ord refty => Ord (Binds refty) where
  compare Binds { bindsContext = ctx1, bindsViews = views1 }
           Binds { bindsContext = ctx2, bindsViews = views2 } =
    case compare ctx1 ctx2 of
      EQ -> compare views1 views2
      out -> out

instance Ord refty => Ord (Defs refty) where
  compare Defs { defsBuilders = builders1, defsTruths = truths1,
                 defsVals = vals1 }
          Defs { defsBuilders = builders2, defsTruths = truths2,
                 defsVals = vals2 } =
    let
      builderlist1 = sort (HashMap.toList builders1)
      builderlist2 = sort (HashMap.toList builders2)
      truthlist1 = sort (HashMap.toList truths1)
      truthlist2 = sort (HashMap.toList truths2)
      valslist1 = sort (HashMap.toList vals1)
      valslist2 = sort (HashMap.toList vals2)
    in
      case compare builderlist1 builderlist2 of
      EQ -> case compare truthlist1 truthlist2 of
        EQ -> compare valslist1 valslist2
        out -> out
      out -> out

instance Ord refty => Ord (Scope refty) where
  compare Scope { scopeInherits = inherits1, scopeDefs = defs1 }
          Scope { scopeInherits = inherits2, scopeDefs = defs2 } =
    let
      inheritslist1 = sort (HashMap.toList inherits1)
      inheritslist2 = sort (HashMap.toList inherits2)
    in
      case compare inheritslist1 inheritslist2 of
        EQ -> compare defs1 defs2
        out -> out

instance Ord refty => Ord (Builder refty) where
  compare Builder { builderKind = kind1, builderVisibility = vis1,
                    builderParams = params1, builderSuperTypes = supers1,
                    builderContent = content1 }
          Builder { builderKind = kind2, builderVisibility = vis2,
                    builderParams = params2, builderSuperTypes = supers2,
                    builderContent = content2 } =
    case compare kind1 kind2 of
      EQ -> case compare vis1 vis2 of
        EQ -> case compare params1 params2 of
          EQ -> case compare supers1 supers2 of
            EQ -> compare content1 content2
            out -> out
          out -> out
        out -> out
      out -> out

instance Ord refty => Ord (Proof refty) where
  compare Proof { proofBody = body1 } Proof { proofBody = body2 } =
    compare body1 body2

instance Ord refty => Ord (Truth refty) where
  compare Truth { truthKind = kind1, truthVisibility = vis1,
                  truthContent = content1, truthProof = proof1 }
          Truth { truthKind = kind2, truthVisibility = vis2,
                  truthContent = content2, truthProof = proof2 } =
    case compare kind1 kind2 of
      EQ -> case compare vis1 vis2 of
        EQ -> case compare proof1 proof2 of
          EQ -> compare content1 content2
          out -> out
        out -> out
      out -> out

instance Ord refty => Ord (Pattern refty) where
  compare Option { optionPats = pats1 } Option { optionPats = pats2 } =
    compare pats1 pats2
  compare Option {} _ = LT
  compare _ Option {} = GT
  compare Deconstruct { deconstructName = name1, deconstructPat = pat1 }
          Deconstruct { deconstructName = name2, deconstructPat = pat2 } =
    case compare name1 name2 of
      EQ -> compare pat1 pat2
      out -> out
  compare Deconstruct {} _ = LT
  compare _ Deconstruct {} = GT
  compare Split { splitFields = fields1, splitStrict = strict1 }
          Split { splitFields = fields2, splitStrict = strict2 } =
    let
      fieldslist1 = sort (HashMap.toList fields1)
      fieldslist2 = sort (HashMap.toList fields2)
    in
      case compare strict1 strict2 of
        EQ -> compare fieldslist1 fieldslist2
        out -> out
  compare Split {} _ = LT
  compare _ Split {} = GT
  compare Typed { typedPat = pat1, typedType = ty1 }
          Typed { typedPat = pat2, typedType = ty2 } =
    case compare pat1 pat2 of
      EQ -> compare ty1 ty2
      out -> out
  compare Typed {} _ = LT
  compare _ Typed {} = GT
  compare As { asName = name1, asPat = pat1 }
          As { asName = name2, asPat = pat2 } =
    case compare name1 name2 of
      EQ -> compare pat1 pat2
      out -> out
  compare As {} _ = LT
  compare _ As {} = GT
  compare Name { nameSym = sym1 } Name { nameSym = sym2 } = compare sym1 sym2
  compare Name {} _ = LT
  compare _ Name {} = GT
  compare Exact { exactLit = lit1 } Exact { exactLit = lit2 } =
    compare lit1 lit2

instance Ord refty => Ord (Exp refty) where
  compare Compound {} Compound {} = _
  compare Compound {} _ = LT
  compare _ Compound {} = GT
  compare Abs { absKind = kind1, absCases = cases1 }
          Abs { absKind = kind2, absCases = cases2 } =
    case compare kind1 kind2 of
      EQ -> compare cases1 cases2
      out -> out
  compare Abs {} _ = LT
  compare _ Abs {} = GT
  compare Match { matchVal = val1, matchCases = cases1 }
          Match { matchVal = val2, matchCases = cases2 } =
    case compare val1 val2 of
      EQ -> compare cases1 cases2
      out -> out
  compare Match {} _ = LT
  compare _ Match {} = GT
  compare Ascribe { ascribeVal = val1, ascribeType = type1 }
          Ascribe { ascribeVal = val2, ascribeType = type2 } =
    case compare val1 val2 of
      EQ -> compare type1 type2
      out -> out
  compare Ascribe {} _ = LT
  compare _ Ascribe {} = GT
  compare Apply { applyFunc = func1, applyArgs = args1 }
          Apply { applyFunc = func2, applyArgs = args2 } =
    case compare func1 func2 of
      EQ -> compare args1 args2
      out -> out
  compare Apply {} _ = LT
  compare _ Apply {} = GT
  compare RecordType { recordTypeFields = fields1 }
          RecordType { recordTypeFields = fields2 } =
    compare fields1 fields2
  compare RecordType {} _ = LT
  compare _ RecordType {} = GT
  compare Record { recordFields = fields1 } Record { recordFields = fields2 } =
    compare fields1 fields2
  compare Record {} _ = LT
  compare _ Record {} = GT
  compare Tuple { tupleFields = fields1 } Tuple { tupleFields = fields2 } =
    compare fields1 fields2
  compare Tuple {} _ = LT
  compare _ Tuple {} = GT
  compare Project { projectVal = val1, projectFields = fields1 }
          Project { projectVal = val2, projectFields = fields2 } =
    case compare fields1 fields2 of
      EQ -> compare val1 val2
      out -> out
  compare Project {} _ = LT
  compare _ Project {} = GT
  compare Sym { symRef = ref1 } Sym { symRef = ref2 } = compare ref1 ref2
  compare Sym {} _ = LT
  compare _ Sym {} = GT
  compare With { withVal = val1, withArgs = args1 }
          With { withVal = val2, withArgs = args2 } =
    case compare val1 val2 of
      EQ -> compare args1 args2
      out -> out
  compare With {} _ = LT
  compare _ With {} = GT
  compare Where { whereVal = val1, whereProp = prop1 }
          Where { whereVal = val2, whereProp = prop2 } =
    case compare val1 val2 of
      EQ -> compare prop1 prop2
      out -> out
  compare Where {} _ = LT
  compare _ Where {} = GT
  compare Anon { anonKind = kind1, anonSuperTypes = supers1,
                 anonParams = params1, anonContent = content1 }
          Anon { anonKind = kind2, anonSuperTypes = supers2,
                 anonParams = params2, anonContent = content2 } =
    case compare kind1 kind2 of
      EQ -> case compare supers1 supers2 of
        EQ -> case compare params1 params2 of
          EQ -> compare content1 content2
          out -> out
        out -> out
      out -> out
  compare Anon {} _ = LT
  compare _ Anon {} = GT
  compare Literal { literalVal = val1 } Literal { literalVal = val2 } =
    compare val1 val2
  compare Literal {} _ = LT
  compare _ Literal {} = GT
  compare BadExp {} BadExp {} = EQ

instance Ord refty => Ord (Entry refty) where
  compare Entry { entryPat = pat1 } Entry { entryPat = pat2 } =
    compare pat1 pat2

instance Ord refty => Ord (Fields refty) where
  compare Fields { fieldsBindings = binds1, fieldsOrder = order1 }
           Fields { fieldsBindings = binds2, fieldsOrder = order2 } =
    let
      bindslist1 = sort (HashMap.toList binds1)
      bindslist2 = sort (HashMap.toList binds2)
    in
      case compare (assocs order1) (assocs order2) of
        EQ -> compare bindslist1 bindslist2
        out -> out

instance Ord refty => Ord (Field refty) where
  compare Field { fieldVal = val1 } Field { fieldVal = val2 } =
    compare val1 val2

instance Ord refty => Ord (Case refty) where
  compare Case { casePat = pat1, caseBody = body1 }
           Case { casePat = pat2, caseBody = body2 } =
    case compare pat1 pat2 of
      EQ -> compare body1 body2
      out -> out

instance Ord refty => Ord (Scopes refty) where
  compare Scopes { scopesArr = arr1 } Scopes { scopesArr = arr2 } =
    compare (assocs arr1) (assocs arr2)

instance Hashable Ref where
  hashWithSalt s Ref { refSymbol = sym, refScopeID = scopeid } =
    s `hashWithSalt` hash sym `hashWithSalt` hash scopeid

instance (Ord refty, Hashable refty) => Hashable (Bind refty) where
  hashWithSalt s Stub { stubType = ty } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` ty
  hashWithSalt s Typeless { typelessVal = val } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` val
  hashWithSalt s Full { fullType = ty, fullVal = val } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` ty `hashWithSalt` val

instance (Ord refty, Hashable refty) => Hashable (Binds refty) where
  hashWithSalt s Binds { bindsContext = ctx, bindsViews = views } =
    s `hashWithSalt` ctx `hashWithSalt` assocs views

instance (Ord refty, Hashable refty) => Hashable (Defs refty) where
  hashWithSalt s Defs { defsBuilders = builders, defsTruths = truths,
                        defsVals = vals } =
    let
      builderlist = sort (HashMap.toList builders)
      truthlist = sort (HashMap.toList truths)
      valslist = sort (HashMap.toList vals)
    in
      s `hashWithSalt` builderlist `hashWithSalt`
      truthlist `hashWithSalt` valslist

instance (Ord refty, Hashable refty) => Hashable (Scope refty) where
  hashWithSalt s Scope { scopeInherits = inherits, scopeDefs = defs } =
    let
      inheritslist = sort (HashMap.toList inherits)
    in
      s `hashWithSalt` inheritslist `hashWithSalt` defs

instance (Ord refty, Hashable refty) => Hashable (Builder refty) where
  hashWithSalt s Builder { builderKind = kind, builderVisibility = vis,
                           builderParams = params, builderSuperTypes = supers,
                           builderContent = content } =
    s `hashWithSalt` kind `hashWithSalt` vis `hashWithSalt`
    params `hashWithSalt` supers `hashWithSalt` content

instance (Ord refty, Hashable refty) => Hashable (Proof refty) where
  hashWithSalt s Proof { proofBody = body } = s `hashWithSalt` body

instance (Ord refty, Hashable refty) => Hashable (Truth refty) where
  hashWithSalt s Truth { truthKind = kind, truthVisibility = vis,
                         truthContent = content, truthProof = proof } =
    s `hashWithSalt` kind `hashWithSalt` vis `hashWithSalt`
    content `hashWithSalt` proof

instance (Ord refty, Hashable refty) => Hashable (Pattern refty) where
  hashWithSalt s Option { optionPats = pats } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` pats
  hashWithSalt s Deconstruct { deconstructName = name, deconstructPat = pat } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` name `hashWithSalt` pat
  hashWithSalt s Split { splitFields = fields, splitStrict = strict } =
    let
      fieldslist = sort (HashMap.toList fields)
    in
      s `hashWithSalt` (2 :: Int) `hashWithSalt`
      strict `hashWithSalt` fieldslist
  hashWithSalt s Typed { typedPat = pat, typedType = ty } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` pat `hashWithSalt` ty
  hashWithSalt s As { asName = name, asPat = pat } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` name `hashWithSalt` pat
  hashWithSalt s Name { nameSym = sym } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` sym
  hashWithSalt s Exact { exactLit = lit } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` lit

instance (Ord refty, Hashable refty) => Hashable (Exp refty) where
  hashWithSalt s Compound {} =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` _
  hashWithSalt s Abs { absKind = kind, absCases = cases } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` kind `hashWithSalt` cases
  hashWithSalt s Match { matchVal = val, matchCases = cases } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` val `hashWithSalt` cases
  hashWithSalt s Ascribe { ascribeVal = val, ascribeType = ty } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` val `hashWithSalt` ty
  hashWithSalt s Apply { applyFunc = func, applyArgs = args } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` func `hashWithSalt` args
  hashWithSalt s RecordType { recordTypeFields = fields } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` fields
  hashWithSalt s Record { recordFields = fields } =
    let
      fieldlist = sort (HashMap.toList fields)
    in
      s `hashWithSalt` (6 :: Int) `hashWithSalt` fieldlist
  hashWithSalt s Tuple { tupleFields = fields } =
    s `hashWithSalt` (7 :: Int) `hashWithSalt` fields
  hashWithSalt s Project { projectVal = val, projectFields = sym } =
    s `hashWithSalt` (8 :: Int) `hashWithSalt` sym `hashWithSalt` val
  hashWithSalt s Sym { symRef = ref } =
    s `hashWithSalt` (9 :: Int) `hashWithSalt` ref
  hashWithSalt s With { withVal = val, withArgs = args } =
    s `hashWithSalt` (10 :: Int) `hashWithSalt` val `hashWithSalt` args
  hashWithSalt s Where { whereVal = val, whereProp = prop } =
    s `hashWithSalt` (11 :: Int) `hashWithSalt` val `hashWithSalt` prop
  hashWithSalt s Anon { anonKind = cls, anonParams = params,
                        anonSuperTypes = supers, anonContent = body } =
    s `hashWithSalt` (12 :: Int) `hashWithSalt` cls `hashWithSalt`
    params `hashWithSalt` supers `hashWithSalt` body
  hashWithSalt s Literal { literalVal = lit } =
    s `hashWithSalt` (13 :: Int) `hashWithSalt` lit
  hashWithSalt s BadExp {} =
    s `hashWithSalt` (14 :: Int)

instance (Ord refty, Hashable refty) => Hashable (Entry refty) where
  hashWithSalt s Entry { entryPat = pat } = s `hashWithSalt` pat

instance (Ord refty, Hashable refty) => Hashable (Fields refty) where
  hashWithSalt s Fields { fieldsBindings = binds, fieldsOrder = order } =
    let
      bindslist = sort (HashMap.toList binds)
    in
      s `hashWithSalt` bindslist `hashWithSalt` assocs order

instance (Ord refty, Hashable refty) => Hashable (Field refty) where
  hashWithSalt s Field { fieldVal = val } = s `hashWithSalt` val

instance (Ord refty, Hashable refty) => Hashable (Case refty) where
  hashWithSalt s Case { casePat = pat, caseBody = body } =
    s `hashWithSalt` pat `hashWithSalt` body

instance (Ord refty, Hashable refty) => Hashable (Scopes refty) where
  hashWithSalt s Scopes { scopesArr = arr } = s `hashWithSalt` assocs arr

instance Functor Bind where
  fmap f b @ Stub { stubType = ty } = b { stubType = fmap f ty }
  fmap f b @ Typeless { typelessVal = val } = b { typelessVal = fmap f val }
  fmap f b @ Full { fullType = ty, fullVal = val } =
    b { fullType = fmap f ty, fullVal = fmap f val }

instance Functor Binds where
  fmap f b @ Binds { bindsViews = views } =
    b { bindsViews = fmap (fmap (fmap f)) views }

instance Functor Defs where
  fmap f d @ Defs { defsBuilders = builders, defsTruths = truths,
                    defsVals = vals } =
    d { defsBuilders = fmap (fmap f) builders,
        defsTruths = fmap (fmap f) truths,
        defsVals = fmap (fmap f) vals }

instance Functor Scope where
  fmap f s @ Scope { scopeInherits = inherits, scopeDefs = defs } =
    s { scopeInherits = fmap (fmap f) inherits, scopeDefs = fmap f defs }

instance Functor Builder where
  fmap f b @ Builder { builderParams = params, builderSuperTypes = supers,
                       builderContent = content } =
    b { builderParams = fmap f params,
        builderSuperTypes = fmap (fmap f) supers,
        builderContent = fmap f content }

instance Functor Proof where
  fmap f p @ Proof { proofBody = body } = p { proofBody = fmap f body }

instance Functor Truth where
  fmap f t @ Truth { truthContent = content, truthProof = proof } =
    t { truthContent = fmap f content, truthProof = fmap (fmap f) proof }

instance Functor Pattern where
  fmap f p @ Option { optionPats = pats } =
    p { optionPats = fmap (fmap f) pats }
  fmap f p @ Deconstruct { deconstructPat = pat } =
    p { deconstructPat = fmap f pat }
  fmap f p @ Split { splitFields = fields } =
    p { splitFields = fmap (fmap f) fields }
  fmap f p @ Typed { typedPat = pat, typedType = ty } =
    p { typedPat = fmap f pat, typedType = fmap f ty }
  fmap f p @ As { asPat = pat } = p { asPat = fmap f pat }
  fmap _ Name { nameSym = sym, namePos = pos } =
    Name { nameSym = sym, namePos = pos }
  fmap _ Exact { exactLit = lit } = Exact { exactLit = lit }

instance Functor Exp where
  fmap f e @ Compound {} = _
  fmap f e @ Abs { absCases = cases } = e { absCases = fmap (fmap f) cases }
  fmap f e @ Match { matchVal = val, matchCases = cases } =
    e { matchVal = fmap f val, matchCases = fmap (fmap f) cases }
  fmap f e @ Ascribe { ascribeVal = val, ascribeType = ty } =
    e { ascribeVal = fmap f val, ascribeType = fmap f ty }
  fmap f e @ Apply { applyFunc = func, applyArgs = args } =
    e { applyFunc = fmap f func, applyArgs = fmap f args }
  fmap f e @ RecordType { recordTypeFields = fields } =
    e { recordTypeFields = fmap f fields }
  fmap f e @ Record { recordFields = fields } =
    e { recordFields = fmap (fmap f) fields }
  fmap f e @ Tuple { tupleFields = fields } =
    e { tupleFields = fmap (fmap f) fields }
  fmap f e @ Project { projectVal = val, projectFields = fields } =
    e { projectVal = fmap f val, projectFields = fmap f fields }
  fmap f e @ Sym { symRef = ref } = e { symRef = f ref }
  fmap f e @ With { withVal = val, withArgs = args } =
    e { withVal = fmap f val, withArgs = fmap f args }
  fmap f e @ Where { whereVal = val, whereProp = prop } =
    e { whereVal = fmap f val, whereProp = fmap f prop }
  fmap f e @ Anon { anonParams = params, anonSuperTypes = supers,
                    anonContent = body } =
    e { anonParams = fmap f params, anonSuperTypes = fmap (fmap f) supers,
        anonContent = fmap f body }
  fmap f Literal { literalVal = lit } = Literal { literalVal = lit }
  fmap f BadExp { badExpPos = pos } = BadExp { badExpPos = pos }

instance Functor Entry where
  fmap f e @ Entry { entryPat = pat } = e { entryPat = fmap f pat }

instance Functor Fields where
  fmap f s @ Fields { fieldsBindings = binds } =
    s { fieldsBindings = fmap (fmap f) binds }

instance Functor Field where
  fmap f d @ Field { fieldVal = val } = d { fieldVal = fmap f val }

instance Functor Case where
  fmap f c @ Case { casePat = pat, caseBody = body } =
    c { casePat = fmap f pat, caseBody = fmap f body }

instance Functor Scopes where
  fmap f s @ Scopes { scopesArr = arr } = s { scopesArr = fmap (fmap f) arr }

instance Foldable Bind where
  foldMap f Stub { stubType = ty } = foldMap f ty
  foldMap f Typeless { typelessVal = val } = foldMap f val
  foldMap f Full { fullType = ty, fullVal = val } =
    foldMap f ty `mappend` foldMap f val

instance Foldable Binds where
  foldMap f Binds { bindsViews = views } = foldMap (foldMap (foldMap f)) views

instance Foldable Defs where
  foldMap f Defs { defsBuilders = builders, defsTruths = truths,
                   defsVals = vals } =
    foldMap (foldMap f) builders `mappend` foldMap (foldMap f) truths `mappend`
    foldMap (foldMap f) vals

instance Foldable Scope where
  foldMap f Scope { scopeInherits = inherits, scopeDefs = defs } =
    foldMap (foldMap f) inherits `mappend` foldMap f defs

instance Foldable Builder where
  foldMap f Builder { builderParams = params, builderSuperTypes = supers,
                      builderContent = content } =
    foldMap f params `mappend` foldMap (foldMap f) supers `mappend`
    foldMap f content

instance Foldable Proof where
  foldMap f Proof { proofBody = body } = foldMap f body

instance Foldable Truth where
  foldMap f Truth { truthContent = content, truthProof = proof } =
    foldMap f content `mappend` foldMap (foldMap f) proof

instance Foldable Pattern where
  foldMap f Option { optionPats = pats } = foldMap (foldMap f) pats
  foldMap f Deconstruct { deconstructPat = pat } = foldMap f pat
  foldMap f Split { splitFields = fields } = foldMap (foldMap f) fields
  foldMap f Typed { typedPat = pat, typedType = ty } =
    foldMap f pat `mappend` foldMap f ty
  foldMap f As { asPat = pat } = foldMap f pat
  foldMap _ Name {} = mempty
  foldMap _ Exact {} = mempty

instance Foldable Exp where
  foldMap f Compound {} = _
  foldMap f Abs { absCases = cases } = foldMap (foldMap f) cases
  foldMap f Match { matchVal = val, matchCases = cases } =
    foldMap f val `mappend` foldMap (foldMap f) cases
  foldMap f Ascribe { ascribeVal = val, ascribeType = ty } =
    foldMap f val `mappend` foldMap f ty
  foldMap f Apply { applyFunc = func, applyArgs = args } =
    foldMap f func `mappend` foldMap f args
  foldMap f e @ RecordType { recordTypeFields = fields } = foldMap f fields
  foldMap f Record { recordFields = fields } = foldMap (foldMap f) fields
  foldMap f Tuple { tupleFields = fields } = foldMap (foldMap f) fields
  foldMap f Project { projectVal = val, projectFields = fields } =
    foldMap f val `mappend` foldMap f fields
  foldMap f Sym { symRef = ref } = f ref
  foldMap f With { withVal = val, withArgs = args } =
    foldMap f val `mappend` foldMap f args
  foldMap f Where { whereVal = val, whereProp = prop } =
    foldMap f val `mappend` foldMap f prop
  foldMap f Anon { anonParams = params, anonSuperTypes = supers,
                   anonContent = body } =
    foldMap f params `mappend` foldMap (foldMap f) supers `mappend`
    foldMap f body
  foldMap _ Literal {} = mempty
  foldMap _ BadExp {} = mempty

instance Foldable Entry where
  foldMap f Entry { entryPat = pat } = foldMap f pat

instance Foldable Fields where
  foldMap f Fields { fieldsBindings = binds } = foldMap (foldMap f) binds

instance Foldable Field where
  foldMap f Field { fieldVal = val } = foldMap f val

instance Foldable Case where
  foldMap f Case { casePat = pat, caseBody = body } =
    foldMap f pat `mappend` foldMap f body

instance Foldable Scopes where
  foldMap f Scopes { scopesArr = arr } = foldMap (foldMap f) arr

instance Traversable Bind where
  traverse f b @ Stub { stubType = ty } =
    (\ty' -> b { stubType = ty' }) <$> traverse f ty
  traverse f b @ Typeless { typelessVal = val } =
    (\val' -> b { typelessVal = val' }) <$> traverse f val
  traverse f b @ Full { fullType = ty, fullVal = val } =
    (\ty' val' -> b { fullType = ty', fullVal = val' }) <$>
      traverse f ty <*> traverse f val

instance Traversable Binds where
  traverse f b @ Binds { bindsViews = views } =
    (\views' -> b { bindsViews = views' }) <$>
      traverse (traverse (traverse f)) views

instance Traversable Defs where
  traverse f d @ Defs { defsBuilders = builders, defsTruths = truths,
                        defsVals = vals } =
    (\builders' truths' vals' -> d { defsBuilders = builders',
                                     defsTruths = truths',
                                     defsVals = vals' }) <$>
      traverse (traverse f) builders <*> traverse (traverse f) truths <*>
      traverse (traverse f) vals

instance Traversable Scope where
  traverse f s @ Scope { scopeInherits = inherits, scopeDefs = defs } =
    (\inherits' defs' -> s { scopeInherits = inherits', scopeDefs = defs' }) <$>
      traverse (traverse f) inherits <*> traverse f defs

instance Traversable Builder where
  traverse f b @ Builder { builderParams = params, builderSuperTypes = supers,
                           builderContent = content } =
    (\params' supers' content' -> b { builderParams = params',
                                      builderSuperTypes = supers',
                                      builderContent = content' }) <$>
      traverse f params <*> traverse (traverse f) supers <*> traverse f content

instance Traversable Proof where
  traverse f p @ Proof { proofBody = body } =
    (\body' -> p { proofBody = body' }) <$> traverse f body

instance Traversable Truth where
  traverse f t @ Truth { truthContent = content, truthProof = proof } =
    (\content' proof' -> t { truthContent = content', truthProof = proof' }) <$>
      traverse f content <*> traverse (traverse f) proof

instance Traversable Pattern where
  traverse f p @ Option { optionPats = pats } =
    (\pats' -> p { optionPats = pats' }) <$> traverse (traverse f) pats
  traverse f p @ Deconstruct { deconstructPat = pat } =
    (\pat' -> p { deconstructPat = pat' }) <$> traverse f pat
  traverse f p @ Split { splitFields = fields } =
    (\fields' -> p { splitFields = fields' }) <$> traverse (traverse f) fields
  traverse f p @ Typed { typedPat = pat, typedType = ty } =
    (\pat' ty' -> p { typedPat = pat', typedType = ty' }) <$>
      traverse f pat <*> traverse f ty
  traverse f p @ As { asPat = pat } =
    (\pat' -> p { asPat = pat' }) <$> traverse f pat
  traverse _ Name { nameSym = sym, namePos = pos } =
    pure Name { nameSym = sym, namePos = pos }
  traverse _ Exact { exactLit = lit } = pure Exact { exactLit = lit }

instance Traversable Exp where
  traverse f e @ Compound {} = _
  traverse f e @ Abs { absCases = cases } =
    (\cases' -> e { absCases = cases' }) <$> traverse (traverse f) cases
  traverse f e @ Match { matchVal = val, matchCases = cases } =
    (\val' cases' -> e { matchVal = val', matchCases = cases' }) <$>
      traverse f val <*> traverse (traverse f) cases
  traverse f e @ Ascribe { ascribeVal = val, ascribeType = ty } =
    (\val' ty' -> e { ascribeVal = val', ascribeType = ty' }) <$>
      traverse f val <*> traverse f ty
  traverse f e @ Apply { applyFunc = func, applyArgs = args } =
    (\func' args' -> e { applyFunc = func', applyArgs = args' }) <$>
      traverse f func <*> traverse f args
  traverse f e @ RecordType { recordTypeFields = fields } =
    (\fields' -> e { recordTypeFields = fields' }) <$> traverse f fields
  traverse f e @ Record { recordFields = fields } =
    (\fields' -> e { recordFields = fields' }) <$> traverse (traverse f) fields
  traverse f e @ Tuple { tupleFields = fields } =
    (\fields' -> e { tupleFields = fields' }) <$> traverse (traverse f) fields
  traverse f e @ Project { projectVal = val, projectFields = fields } =
    (\val' fields' -> e { projectVal = val', projectFields = fields' }) <$>
      traverse f val <*> traverse f fields
  traverse f e @ Sym { symRef = ref } = (\ref' -> e { symRef = ref' }) <$> f ref
  traverse f e @ With { withVal = val, withArgs = args } =
    (\val' args' -> e { withVal = val', withArgs = args' }) <$>
      traverse f val <*> traverse f args
  traverse f e @ Where { whereVal = val, whereProp = prop } =
    (\val' prop' -> e { whereVal = val', whereProp = prop' }) <$>
      traverse f val <*> traverse f prop
  traverse f e @ Anon { anonParams = params, anonSuperTypes = supers,
                        anonContent = body } =
    (\params' supers' body' ->
      e { anonParams = params', anonSuperTypes = supers,
          anonContent = body' }) <$> traverse f params <*>
    traverse (traverse f) supers <*> traverse f body
  traverse _ Literal { literalVal = lit } = pure Literal { literalVal = lit }
  traverse _ BadExp { badExpPos = pos } = pure BadExp { badExpPos = pos }

instance Traversable Entry where
  traverse f e @ Entry { entryPat = pat } =
    (\pat' -> e { entryPat = pat' }) <$> traverse f pat

instance Traversable Fields where
  traverse f s @ Fields { fieldsBindings = bindings } =
    (\bindings' -> s { fieldsBindings = bindings' }) <$>
      traverse (traverse f) bindings

instance Traversable Field where
  traverse f d @ Field { fieldVal = val } =
    (\val' -> d { fieldVal = val'}) <$> traverse f val

instance Traversable Case where
  traverse f c @ Case { casePat = pat, caseBody = body } =
    (\pat' body' -> c { casePat = pat', caseBody = body' }) <$>
      traverse f pat <*> traverse f body

instance Traversable Scopes where
  traverse f s @ Scopes { scopesArr = scopes } =
    (\scopes' -> s { scopesArr = scopes' }) <$> traverse (traverse f) scopes

instance (GenericXMLString tag, Show tag, GenericXMLString text) =>
         XmlPickler [NodeG [] tag text] ScopeID where
  xpickle =
    xpWrap (\(sym, scope) -> Ref { refSymbol = sym, refScopeID = scope },
            \Ref { refSymbol = sym, refScopeID = scope } -> (sym, scope))
           (xpElemAttrs (gxFromString "Ref") (xpPair xpickle xpickle))
