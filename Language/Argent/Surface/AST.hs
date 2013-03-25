-- Copyright (c) 2013 Eric McCorkle.
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
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- | The Abstract Syntax Tree, as yielded by a parser.  This is
-- rendered into surface syntax by Collect, which gathers all entries
-- into a table.
--
-- Note that this structure isn't meant to be processed in any truly
-- meaningful way.
module Language.Argent.Surface.AST(
       ScopeClass(..),
       Decl(..),
       Compound(..),
       Pattern(..),
       Exp(..),
       Entry(..),
       Binding(..),
       Case(..)
       ) where

import Control.Applicative
import Data.Foldable
import Data.Hash
import Data.Monoid hiding ((<>))
import Data.Pos
import Data.Traversable
import Language.Argent.Surface.Common
import Prelude hiding (sequence)
import Prelude.Extras(Eq1(..), Ord1(..))
import Prelude.Extras.ExtraInstances()
import Test.QuickCheck
import Text.Format

-- | Declarations.  These represent static declarations inside a
-- scope.  Note: some of these can be built from others.
data Decl
       -- | The type of a symbol.
       sym =
    -- | Scope entities.  This is a common structure for a number of
    -- entity declarations that all have the same structure: modules,
    -- signatures, and classes.
    Scope {
      -- | The name of the scope.
      scopeName :: !sym,
      -- | The type of entity the scope represents.
      scopeClass :: !ScopeClass,
      -- | The declared supertypes for this scope entity.
      scopeSuperTypes :: [Exp sym],
      -- | The parameters of the scope entity.
      scopeParams :: [Binding sym],
      -- | The entities declared by the scope.
      scopeBody :: [Decl sym],
      -- | The position in source from which this arises.
      scopePos :: !Pos
    }
    -- | Value entities.  These are (possibly mutable) values defined
    -- in a scope.  These include function declarations.
  | Value {
      -- | The name of the value.
      valueName :: !sym,
      -- | Whether or not the value is mutable.
      valueMutable :: !Bool,
      -- | The value's type.  Note: not all declarations will
      -- necessarily have a type associated with them.
      valueType :: Maybe (Exp sym),
      -- | The value's initializer.
      valueInit :: Exp sym,
      -- | The position in source from which this arises.
      valuePos :: !Pos
    }
    -- | Invariants.  These are propositions, which may reference
    -- names defined in the given scope, that are added by conjunction
    -- to every pre- and post-condition for every action within the
    -- scope.
  | Invariant {
      -- | The name of the invariant.
      invName :: !sym,
      -- | The invariant proposition.
      invProp :: Exp sym,
      -- | The position in source from which this arises.
      invPos :: !Pos
    }

-- | Compound expression elements.  These are either "ordinary"
-- expressions, or declarations.
data Compound
       -- | The type of a symbol.
       sym =
    -- | An ordinary expression.
    Exp !(Exp sym)
    -- | A declaration.
  | Decl !(Decl sym)

-- | A pattern, for pattern match expressions.
data Pattern
       -- | The type of a symbol
       sym =
    -- | A constructor.  Mirrors a call expression.
    Construct {
      -- | The name of the constructor.
      constructName :: sym,
      -- | Whether or not the binding is strict (ie. it omits some args)
      constructStrict :: !Bool,
      -- | The arguments to the constructor.
      constructArgs :: [Entry Pattern sym],
      -- | The position in source from which this arises.
      constructPos :: !Pos
    }
    -- | A projection.  Mirrors a record expression.
  | Project {
      -- | The fields being projected.
      projectFields :: [Entry Pattern sym],
      -- | Whether or not the binding is strict (ie. it omits some fields)
      projectStrict :: !Bool,
      -- | The position in source from which this arises.
      projectPos :: !Pos
    }
    -- | A typed pattern.  Fixes the type of a pattern.
  | Typed {
      -- | The pattern whose type is being fixed.
      typedPat :: Pattern sym,
      -- | The type to which the pattern is fixed.
      typedType :: Exp sym,
      -- | The position in source from which this arises.
      typedPos :: !Pos
    }
    -- | An as pattern.  Captures part of a pattern as a name, but
    -- continues to match.
  | As {
      -- | The name to bind.
      asName :: !sym,
      -- | The sub-pattern to match.
      asPat :: Pattern sym,
      -- | The position in source from which this arises.
      asPos :: !Pos
    }
    -- | A name binding.  Binds the given name to the contents.
  | Name {
      -- | The name to bind.
      nameSym :: !sym,
      -- | The position in source from which this arises.
      namePos :: !Pos
    }

-- | Expressions.  These represent computed values of any type.
data Exp
       -- | The type of a symbol.
       sym =
    -- | An expression that may contain declarations as well as
    -- expressions.
    Compound {
      -- | The body of the compound expression.
      compoundBody :: [Compound sym],
      -- | The position in source from which this arises.
      compoundPos :: !Pos
    }
    -- | Match statement.  Given a set of (possibly typed) patterns,
    -- find the meet of all types above the argument's type in the
    -- cases, then apply the first pattern for the meet type that
    -- matches the argument.
  | Match {
      -- | The argument to the match statement.
      matchVal :: Exp sym,
      -- | The cases, in order.
      matchCases :: [Case sym],
      -- | The position in source from which this arises.
      matchPos :: !Pos
    }
    -- | Function expression (lambda).  Has the same form and
    -- semantics as a match statement (in fact, match statements are
    -- transliterated into calls to constant functions).
  | Func {
      -- | The cases for the function.
      funcCases :: [Case sym],
      -- | The position in source from which this arises.
      funcPos :: !Pos
    }
    -- | Ascribe expression.  Fixes the type of a given expression.
  | Ascribe {
      -- | The expression whose type is being set.
      ascribeVal :: Exp sym,
      -- | The type.
      ascribeType :: Exp sym,
      -- | The position in source from which this arises.
      ascribePos :: !Pos
    }
    -- | A sequence of expressions.  This represents a function call,
    -- possibly with inorder symbols.  This is re-parsed once the
    -- inorder symbols are known.
  | Seq {
      -- | The sequence of expressions.
      seqVals :: [Exp sym],
      -- | The position in source from which this arises.
      seqPos :: !Pos
    }
    -- | A record literal.  Note that we might see mixed tuple-style
    -- and record style.  This construct is also used to encode
    -- structure types.
  | Record {
      -- | The fields in this record expression.
      recFields :: [Entry Exp sym],
      -- | The position in source from which this arises.
      recPos :: !Pos
    }
    -- | Reference to a name.  Note: this is all handled with the
    -- Bound framework.
  | Sym {
      -- | The name being referenced.
      symName :: !sym,
      -- | The position in source from which this arises.
      symPos :: !Pos
    }

-- | An entry in a record field or call argument list.
data Entry
       -- | The type constructor to build the content.
       con
       -- | The type of a symbol.
       sym =
    -- | A named field.  This sets a specific field or argument to a
    -- certain value.
    Named {
      -- | The name of the field or argument being set.
      namedName :: !sym,
      -- | The value to which the field or argument is set.
      namedVal :: con sym,
      -- | The position in source from which this arises.
      namedPos :: !Pos
    }
    -- | Unnamed field.  This is just an expression.
  | Unnamed (con sym)

-- | A simple name binding.
data Binding
       -- | The type of a symbol.
       sym =
  Binding {
    -- | The name being bound.
    bindingName :: !sym,
    -- | The type assigned to the bound name.
    bindingType :: Exp sym,
    -- | The position in source from which this arises.
    bindingPos :: !Pos
  }

-- | A case in a match statement or a function definition.
data Case
      -- | The type of a symbol.
      sym =
  Case {
    -- | The pattern to match for this case.
    casePat :: Pattern sym,
    -- | The expression to execute for this case.
    caseBody :: Exp sym,
    -- | The position in source from which this arises.
    casePos :: !Pos
  }

instance Eq1 Decl where
  (Scope { scopeName = name1, scopeClass = cls1, scopeParams = params1,
           scopeSuperTypes = supers1, scopeBody = body1 }) ==#
    (Scope { scopeName = name2, scopeClass = cls2, scopeParams = params2,
             scopeSuperTypes = supers2, scopeBody = body2 }) =
    (name1 == name2) && (cls1 == cls2) && (params1 ==# params2) &&
    (supers1 ==# supers2) && (body1 ==# body2)
  (Value { valueName = name1, valueMutable = mut1,
           valueType = ty1, valueInit = init1 }) ==#
    (Value { valueName = name2, valueMutable = mut2,
             valueType = ty2, valueInit = init2 }) =
    (name1 == name2) && (mut1 == mut2) && (ty1 == ty2) && (init1 == init2)
  (Invariant { invName = name1, invProp = prop1 }) ==#
    (Invariant { invName = name2, invProp = prop2 }) =
    (name1 == name2) && (prop1 ==# prop2)
  _ ==# _ = False

instance Eq1 Compound where
  Exp e1 ==# Exp e2 = e1 ==# e2
  Decl d1 ==# Decl d2 = d1 ==# d2
  _ ==# _ = False

instance Eq1 Pattern where
  (Construct { constructName = name1, constructStrict = strict1,
               constructArgs = args1 }) ==#
    (Construct { constructName = name2, constructStrict = strict2,
                 constructArgs = args2 }) =
      (strict1 == strict2) && (name1 == name2) && (args1 == args2)
  (Project { projectFields = fields1, projectStrict = strict1 }) ==#
    (Project { projectFields = fields2, projectStrict = strict2 }) =
      (fields1 ==# fields2) && (strict1 == strict2)
  (Typed { typedPat = pat1, typedType = ty1 }) ==#
    (Typed { typedPat = pat2, typedType = ty2 }) =
      (pat1 ==# pat2) && (ty1 ==# ty2)
  (As { asName = name1, asPat = pat1 }) ==#
    (As { asName = name2, asPat = pat2 }) =
      (name1 == name2) && (pat1 ==# pat2)
  (Name { nameSym = name1 }) ==# (Name { nameSym = name2 }) = name1 == name2
  _ ==# _ = False

instance Eq1 Exp where
  (Sym { symName = name1 }) ==# (Sym { symName = name2 }) = name1 == name2
  (Seq { seqVals = vals1 }) ==# (Seq { seqVals = vals2 }) = vals1 ==# vals2
  (Record { recFields = fields1 }) ==# (Record { recFields = fields2 }) =
    fields1 ==# fields2  
  (Ascribe { ascribeVal = val1, ascribeType = ty1 }) ==#
    (Ascribe { ascribeVal = val2, ascribeType = ty2 }) =
      (val1 ==# val2) && (ty1 ==# ty2)
  (Func { funcCases = cases1 }) ==#
    (Func { funcCases = cases2 }) = cases1 ==# cases2
  (Match { matchVal = val1, matchCases = cases1 }) ==#
    (Match { matchVal = val2, matchCases = cases2 }) =
      (val1 ==# val2) && (cases1 ==# cases2)
  (Compound { compoundBody = body1 }) ==#
    (Compound { compoundBody = body2 }) = body1 ==# body2
  _ ==# _ = False

instance Eq1 con => Eq1 (Entry con) where
  (Named { namedName = name1, namedVal = val1 }) ==#
    (Named { namedName = name2, namedVal = val2 }) =
    (name1 == name2) && (val1 ==# val2)
  (Unnamed e1) ==# (Unnamed e2) = e1 ==# e2
  _ ==# _ = False

instance Eq1 Binding where
  (Binding { bindingName = name1, bindingType = ty1 }) ==#
    (Binding { bindingName = name2, bindingType = ty2 }) =
    (name1 == name2) && (ty1 ==# ty2)

instance Eq1 Case where
  (Case { caseBody = body1, casePat = pat1 }) ==#
    (Case { caseBody = body2, casePat = pat2 }) =
      (body1 ==# body2) && (pat1 ==# pat2)

instance Eq sym => Eq (Decl sym) where (==) = (==#)
instance Eq sym => Eq (Compound sym) where (==) = (==#)
instance Eq sym => Eq (Exp sym) where (==) = (==#)
instance Eq sym => Eq (Binding sym) where (==) = (==#)
instance Eq sym => Eq (Case sym) where (==) = (==#)
instance (Eq1 con, Eq sym) => Eq (Entry con sym) where (==) = (==#)

instance Ord1 Decl where
  compare1 (Scope { scopeName = name1, scopeClass = cls1, scopeBody = body1,
                    scopeParams = params1, scopeSuperTypes = supers1 })
           (Scope { scopeName = name2, scopeClass = cls2, scopeBody = body2,
                    scopeParams = params2, scopeSuperTypes = supers2 }) =
    case compare name1 name2 of
      EQ -> case compare cls1 cls2 of
        EQ -> case compare1 params1 params2 of
          EQ -> case compare1 supers1 supers2 of
            EQ -> compare1 body1 body2
            out -> out
          out -> out
        out -> out
      out -> out
  compare1 (Scope {}) _ = GT
  compare1 _ (Scope {}) = LT
  compare1 (Value { valueName = name1, valueMutable = mut1,
                    valueType = ty1, valueInit = init1 })
           (Value { valueName = name2, valueMutable = mut2,
                    valueType = ty2, valueInit = init2 }) =
    case compare name1 name2 of
      EQ -> case compare mut1 mut2 of
        EQ -> case compare1 ty1 ty2 of
          EQ -> compare1 init1 init2
          out -> out
        out -> out
      out -> out
  compare1 (Value {}) _ = GT
  compare1 _ (Value {}) = LT
  compare1 (Invariant { invName = name1, invProp = prop1 })
           (Invariant { invName = name2, invProp = prop2 }) =
    case compare name1 name2 of
      EQ -> compare1 prop1 prop2
      out -> out

instance Ord1 Compound where
  compare1 (Exp e1) (Exp e2) = compare1 e1 e2
  compare1 (Exp _) _ = GT
  compare1 _ (Exp _) = LT
  compare1 (Decl d1) (Decl d2) = compare1 d1 d2

instance Ord1 Exp where
  compare1 (Compound { compoundBody = body1 })
           (Compound { compoundBody = body2 }) =
    compare body1 body2
  compare1 (Compound {}) _ = GT
  compare1 _ (Compound {}) = LT
  compare1 (Func { funcCases = cases1 }) (Func { funcCases = cases2 }) = 
    compare1 cases1 cases2
  compare1 (Func {}) _ = GT
  compare1 _ (Func {}) = LT
  compare1 (Match { matchVal = val1, matchCases = cases1 })
           (Match { matchVal = val2, matchCases = cases2 }) =
    case compare1 val1 val2 of
      EQ -> compare1 cases1 cases2
      out -> out
  compare1 (Match {}) _ = GT
  compare1 _ (Match {}) = LT
  compare1 (Seq { seqVals = vals1 }) (Seq { seqVals = vals2 }) =
    compare vals1 vals2
  compare1 (Seq {}) _ = GT
  compare1 _ (Seq {}) = LT
  compare1 (Record { recFields = fields1 }) (Record { recFields = fields2 }) =
    compare1 fields1 fields2
  compare1 (Record {}) _ = GT
  compare1 _ (Record {}) = LT
  compare1 (Ascribe { ascribeVal = val1, ascribeType = ty1 })
           (Ascribe { ascribeVal = val2, ascribeType = ty2 }) =
    case compare val1 val2 of
      EQ -> compare ty1 ty2
      out -> out
  compare1 (Ascribe {}) _ = GT
  compare1 _ (Ascribe {}) = LT
  compare1 (Sym { symName = name1 }) (Sym { symName = name2 }) =
    compare name1 name2

instance Ord1 Pattern where
  compare1 (Construct { constructName = name1, constructStrict = strict1,
                        constructArgs = args1 })
           (Construct { constructName = name2, constructStrict = strict2,
                        constructArgs = args2 }) =
    case compare strict1 strict2 of
      EQ -> case compare name1 name2 of
        EQ -> compare1 args1 args2
        out -> out
      out -> out
  compare1 (Construct {}) _ = GT
  compare1 _ (Construct {}) = LT
  compare1 (Project { projectFields = fields1, projectStrict = strict1 })
           (Project { projectFields = fields2, projectStrict = strict2 }) =
    case compare strict1 strict2 of
      EQ -> compare1 fields1 fields2
      out -> out
  compare1 (Project {}) _ = GT
  compare1 _ (Project {}) = LT
  compare1 (Typed { typedPat = pat1, typedType = ty1 })
           (Typed { typedPat = pat2, typedType = ty2 }) =
    case compare1 pat1 pat2 of
      EQ -> compare1 ty1 ty2
      out -> out
  compare1 (Typed {}) _ = GT
  compare1 _ (Typed {}) = LT
  compare1 (As { asName = name1, asPat = pat1 })
           (As { asName = name2, asPat = pat2 }) =
    case compare name1 name2 of
      EQ -> compare1 pat1 pat2
      out -> out
  compare1 (As {}) _ = GT
  compare1 _ (As {}) = LT
  compare1 (Name { nameSym = name1 }) (Name { nameSym = name2 }) =
    compare name1 name2

instance Ord1 Binding where
  compare1 (Binding { bindingName = name1, bindingType = ty1 })
           (Binding { bindingName = name2, bindingType = ty2 }) =
    case compare name1 name2 of
      EQ -> compare1 ty1 ty2
      out -> out

instance Ord1 Case where
  compare1 (Case { casePat = pat1, caseBody = body1 })
           (Case { casePat = pat2, caseBody = body2 }) =
    case compare1 pat1 pat2 of
      EQ -> compare1 body1 body2
      out -> out

instance Ord1 con => Ord1 (Entry con) where
  compare1 (Named { namedName = name1, namedVal = val1 })
           (Named { namedName = name2, namedVal = val2 }) =
    case compare name1 name2 of
      EQ -> compare1 val1 val2
      out -> out
  compare1 (Named {}) _ = GT
  compare1 _ (Named {}) = LT
  compare1 (Unnamed e1) (Unnamed e2) = compare1 e1 e2

instance Ord sym => Ord (Decl sym) where compare = compare1
instance Ord sym => Ord (Compound sym) where compare = compare1
instance Ord sym => Ord (Exp sym) where compare = compare1
instance Ord sym => Ord (Binding sym) where compare = compare1
instance Ord sym => Ord (Case sym) where compare = compare1
instance (Ord1 con, Ord sym) => Ord (Entry con sym) where compare = compare1

instance Position (Decl sym) where
  pos (Scope { scopePos = p }) = p
  pos (Value { valuePos = p }) = p
  pos (Invariant { invPos = p }) = p

instance Position (Compound sym) where
  pos (Decl d) = pos d
  pos (Exp e) = pos e

instance Position (Exp sym) where
  pos (Compound { compoundPos = p }) = p
  pos (Func { funcPos = p }) = p
  pos (Match { matchPos = p }) = p
  pos (Ascribe { ascribePos = p }) = p
  pos (Seq { seqPos = p }) = p
  pos (Record { recPos = p }) = p
  pos (Sym { symPos = p }) = p

instance Position (Pattern sym) where
  pos (Construct { constructPos = p }) = p
  pos (Project { projectPos = p }) = p
  pos (Typed { typedPos = p }) = p
  pos (As { asPos = p }) = p
  pos (Name { namePos = p }) = p

instance Position (Binding sym) where
  pos (Binding { bindingPos = p }) = p

instance Position (Case sym) where
  pos (Case { casePos = p }) = p

instance Position (con sym) => Position (Entry con sym) where
  pos (Named { namedPos = p }) = p
  pos (Unnamed e) = pos e

instance Hashable sym => Hashable (Decl sym) where
  hash (Scope { scopeName = name, scopeClass = cls, scopeParams = params,
                scopeSuperTypes = supers, scopeBody = body }) =
    hashInt 1 `combine` hash name `combine` hash cls `combine`
    hash params `combine` hash supers `combine` hash body `combine` hash body
  hash (Value { valueName = name, valueMutable = mutable,
                valueType = ty, valueInit = val }) =
    hashInt 2 `combine` hash name `combine`
    hash mutable `combine` hash ty `combine` hash val
  hash (Invariant { invName = name, invProp = prop }) =
    hashInt 3 `combine` hash name `combine` hash prop

instance Hashable sym => Hashable (Compound sym) where
  hash (Decl d) = hashInt 1 `combine` hash d
  hash (Exp e) = hashInt 2 `combine` hash e

instance Hashable sym => Hashable (Exp sym) where
  hash (Compound { compoundBody = body }) = hashInt 1 `combine` hash body
  hash (Func { funcCases = cases }) = hashInt 2 `combine` hash cases
  hash (Match { matchVal = val, matchCases = cases }) =
    hashInt 3 `combine` hash val `combine` hash cases
  hash (Ascribe { ascribeVal = val, ascribeType = ty }) =
    hashInt 4 `combine` hash val `combine` hash ty
  hash (Seq { seqVals = vals }) = hashInt 5 `combine` hash vals
  hash (Record { recFields = fields }) = hashInt 6 `combine` hash fields
  hash (Sym { symName = name }) = hashInt 7 `combine` hash name

instance Hashable sym => Hashable (Pattern sym) where
  hash (Construct { constructName = name, constructStrict = strict,
                    constructArgs = args }) =
    hashInt 1 `combine` hash name `combine` hash strict `combine` hash args
  hash (Project { projectStrict = strict, projectFields = fields }) =
    hashInt 2 `combine` hash strict `combine` hash fields
  hash (Typed { typedPat = pat, typedType = ty }) =
    hashInt 3 `combine` hash pat `combine` hash ty
  hash (As { asName = name, asPat = pat }) =
    hashInt 4 `combine` hash name `combine` hash pat
  hash (Name { nameSym = name }) = hashInt 4 `combine` hash name

instance Hashable sym => Hashable (Binding sym) where
  hash (Binding { bindingName = name, bindingType = ty }) =
    hash name `combine` hash ty

instance Hashable sym => Hashable (Case sym) where
  hash (Case { casePat = pat, caseBody = body }) =
    hash pat `combine` hash body

instance (Hashable sym, Hashable (con sym)) => Hashable (Entry con sym) where
  hash (Named { namedName = name, namedVal = val }) =
    hashInt 1 `combine` hash name `combine` hash val
  hash (Unnamed e) = hashInt 2 `combine` hash e

instance Functor Decl where
  fmap f d @ (Scope { scopeName = name, scopeSuperTypes = supers,
                      scopeParams = params, scopeBody = body }) =
    d { scopeName = f name, scopeSuperTypes = fmap (fmap f) supers,
        scopeParams = fmap (fmap f) params, scopeBody = fmap (fmap f) body }
  fmap f d @ (Value { valueName = name, valueInit = val, valueType = ty }) =
    d { valueName = f name, valueInit = fmap f val,
        valueType = fmap (fmap f) ty }
  fmap f d @ (Invariant { invName = name, invProp = prop }) =
    d { invName = f name, invProp = fmap f prop }

instance Functor Compound where
  fmap f (Decl d) = Decl (fmap f d)
  fmap f (Exp e) = Exp (fmap f e)

instance Functor Exp where
  fmap f e @ (Sym { symName = name }) = e { symName = f name }
  fmap f e @ (Seq { seqVals = vals }) = e { seqVals = fmap (fmap f) vals }
  fmap f e @ (Record { recFields = fields }) =
    e { recFields = fmap (fmap f) fields }
  fmap f e @ (Ascribe { ascribeVal = val, ascribeType = ty }) =
    e { ascribeVal = fmap f val, ascribeType = fmap f ty }
  fmap f e @ (Match { matchVal = val, matchCases = cases }) =
    e { matchVal = fmap f val, matchCases = fmap (fmap f) cases }
  fmap f e @ (Func { funcCases = cases }) =
    e { funcCases = fmap (fmap f) cases }
  fmap f e @ (Compound { compoundBody = body }) =
    e { compoundBody = fmap (fmap f) body }

instance Functor Pattern where
  fmap f p @ (Construct { constructName = name, constructArgs = args }) =
    p { constructName = f name, constructArgs = fmap (fmap f) args }
  fmap f p @ (Project { projectFields = fields }) =
    p { projectFields = fmap (fmap f) fields }
  fmap f p @ (Typed { typedPat = pat, typedType = ty }) =
    p { typedPat = fmap f pat, typedType = fmap f ty }
  fmap f p @ (As { asName = name, asPat = pat }) =
    p { asName = f name, asPat = fmap f pat }
  fmap f p @ (Name { nameSym = name }) = p { nameSym = f name }

instance Functor Binding where
  fmap f b @ (Binding { bindingName = name, bindingType = ty }) =
    b { bindingName = f name, bindingType = fmap f ty }

instance Functor Case where
  fmap f b @ (Case { casePat = pat, caseBody = body }) =
    b { casePat = fmap f pat, caseBody = fmap f body }

instance Functor con => Functor (Entry con) where
  fmap f e @ (Named { namedName = name, namedVal = val }) =
    e { namedName = f name, namedVal = fmap f val }
  fmap f (Unnamed e) = Unnamed (fmap f e)

instance Foldable Decl where
  foldMap f (Scope { scopeName = name, scopeSuperTypes = supers,
                     scopeParams = params, scopeBody = body }) =
    f name `mappend` foldMap (foldMap f) supers `mappend`
    foldMap (foldMap f) params `mappend` foldMap (foldMap f) body
  foldMap f (Value { valueName = name, valueInit = val, valueType = ty }) =
    f name `mappend` foldMap f val `mappend` foldMap (foldMap f) ty
  foldMap f (Invariant { invName = name, invProp = prop }) =
    f name `mappend` foldMap f prop

instance Foldable Compound where
  foldMap f (Decl d) = foldMap f d
  foldMap f (Exp e) = foldMap f e

instance Foldable Exp where
  foldMap f (Sym { symName = name }) = f name
  foldMap f (Seq { seqVals = vals }) = foldMap (foldMap f) vals
  foldMap f (Record { recFields = fields }) = foldMap (foldMap f) fields
  foldMap f (Ascribe { ascribeVal = val, ascribeType = ty }) =
    foldMap f val `mappend` foldMap f ty
  foldMap f (Func { funcCases = cases }) = foldMap (foldMap f) cases
  foldMap f (Match { matchVal = val, matchCases = cases }) =
    foldMap f val `mappend` foldMap (foldMap f) cases
  foldMap f (Compound { compoundBody = body }) = foldMap (foldMap f) body

instance Foldable Pattern where
  foldMap f (Construct { constructName = name, constructArgs = args }) =
    f name `mappend` foldMap (foldMap f) args
  foldMap f (Project { projectFields = fields }) = foldMap (foldMap f) fields
  foldMap f (Typed { typedPat = pat, typedType = ty }) =
    foldMap f pat `mappend` foldMap f ty
  foldMap f (As { asName = name, asPat = pat }) =
    f name `mappend` foldMap f pat
  foldMap f (Name { nameSym = name }) = f name

instance Foldable Binding where
  foldMap f (Binding { bindingName = name, bindingType = ty }) =
    f name `mappend` foldMap f ty

instance Foldable Case where
  foldMap f (Case { casePat = pat, caseBody = body }) =
    foldMap f pat `mappend` foldMap f body

instance Foldable con => Foldable (Entry con) where
  foldMap f (Named { namedName = name, namedVal = val }) =
    f name `mappend` foldMap f val
  foldMap f (Unnamed e) = foldMap f e

instance Traversable Decl where
  traverse f d @ (Scope { scopeName = name, scopeSuperTypes = supers,
                      scopeParams = params, scopeBody = body }) =
    (\name' supers' params' body' ->
      d { scopeName = name', scopeSuperTypes = supers',
        scopeParams = params', scopeBody = body' }) <$>
      f name <*> traverse (traverse f) supers <*>
      traverse (traverse f) params <*> traverse (traverse f) body 
  traverse f d @ (Value { valueName = name, valueInit = val, valueType = ty }) =
    (\name' val' ty' ->
      d { valueName = name', valueInit = val', valueType = ty' }) <$>
      f name <*> traverse f val <*> traverse (traverse f) ty
  traverse f d @ (Invariant { invName = name, invProp = prop }) =
    (\name' prop' -> d { invName = name', invProp = prop' }) <$>
      f name <*> traverse f prop

instance Traversable Compound where
  traverse f (Decl d) = Decl <$> traverse f d
  traverse f (Exp e) = Exp <$> traverse f e

instance Traversable Exp where
  traverse f e @ (Sym { symName = name }) =
    (\name' -> e { symName = name' }) <$> f name
  traverse f e @ (Ascribe { ascribeVal = val, ascribeType = ty }) =
    (\val' ty' -> e { ascribeVal = val', ascribeType = ty' }) <$>
      traverse f val <*> traverse f ty
  traverse f e @ (Seq { seqVals = vals }) =
    (\vals' -> e { seqVals = vals' }) <$> traverse (traverse f) vals
  traverse f e @ (Record { recFields = fields }) =
    (\fields' -> e { recFields = fields' }) <$> traverse (traverse f) fields
  traverse f e @ (Func { funcCases = cases }) =
    (\cases' -> e { funcCases = cases' }) <$> traverse (traverse f) cases
  traverse f e @ (Match { matchVal = val, matchCases = cases }) =
    (\val' cases' -> e { matchVal = val', matchCases = cases' }) <$>
      traverse f val <*> traverse (traverse f) cases
  traverse f e @ (Compound { compoundBody = body }) =
    (\body' -> e { compoundBody = body' }) <$> traverse (traverse f) body

instance Traversable Pattern where
  traverse f p @ (Construct { constructName = name, constructArgs = args }) =
    (\name' args' -> p { constructName = name', constructArgs = args' }) <$>
      f name <*> traverse (traverse f) args
  traverse f p @ (Project { projectFields = fields }) =
    (\fields' -> p { projectFields = fields' }) <$>
      traverse (traverse f) fields
  traverse f p @ (Typed { typedPat = pat, typedType = ty }) =
    (\pat' ty' -> p { typedPat = pat', typedType = ty' }) <$>
      traverse f pat <*> traverse f ty
  traverse f p @ (As { asName = name, asPat = pat }) =
    (\name' pat' -> p { asName = name', asPat = pat' }) <$>
      f name <*> traverse f pat
  traverse f p @ (Name { nameSym = name }) =
    (\name' -> p { nameSym = name' }) <$> f name

instance Traversable Binding where
  traverse f b @ (Binding { bindingName = name, bindingType = ty }) =
    (\name' ty' -> b { bindingName = name', bindingType = ty' }) <$>
      f name <*> traverse f ty

instance Traversable Case where
  traverse f b @ (Case { casePat = pat, caseBody = body }) =
    (\pat' body' -> b { casePat = pat', caseBody = body' }) <$>
      traverse f pat <*> traverse f body

instance Traversable con => Traversable (Entry con) where
  traverse f e @ (Named { namedName = name, namedVal = val }) =
    (\name' val' -> e { namedName = name', namedVal = val' }) <$>
      f name <*> traverse f val
  traverse f (Unnamed e) = Unnamed <$> traverse f e

instance Format sym => Format (Decl sym) where
  format (Scope { scopeName = name, scopeClass = cls,
                  scopeSuperTypes = supers, scopeParams = params,
                  scopeBody = body }) =
    let
      header = show cls <+> name
      withparams = case params of
        [] -> header
        params' -> parenList header params'
      withsupers = case supers of
        [] -> withparams
        supers' -> withparams <+> colon <+>
          (nest 2 (sep (punctuate comma supers')))
    in
      braceBlock withsupers body
  format (Value { valueName = name, valueMutable = True,
                  valueType = Just ty, valueInit = val }) =
    name <+> colon <+> nest 2 (sep [format "mutable", format ty,
                                    equals, format val])
  format (Value { valueName = name, valueMutable = False,
                  valueType = Just ty, valueInit = val }) =
    name <+> colon <+> nest 2 (sep [format ty, equals, format val])
  format (Value { valueName = name, valueMutable = True,
                  valueType = Nothing, valueInit = val }) =
    name <+> colon <+> nest 2 (sep [format "mutable", equals, format val])
  format (Value { valueName = name, valueMutable = False,
                  valueType = Nothing, valueInit = val }) =
    name <+> equals <+> nest 2 val
  format (Invariant { invName = name, invProp = prop }) =
    hang (format "invariant" <+> name <+> equals) 2 prop

instance Format sym => Format (Compound sym) where
  format (Decl d) = format d
  format (Exp e) = format e

instance Format sym => Format (Exp sym) where
  format (Compound { compoundBody = body }) =
    block 2 (lbrace) (sep body) rbrace
  format (Func { funcCases = cases }) = (sep (punctuate (format "|") cases))
  format (Match { matchVal = val, matchCases = cases }) =
    hang (format "match" <+> val) 2 (sep (punctuate (format "|") cases))
  format (Ascribe { ascribeVal = val, ascribeType = ty }) =
    hang (val <+> colon) 2 ty
  format (Seq { seqVals = vals }) = nest 2 (sep vals)
  format (Record { recFields = fields }) =
    lparen <> (nest 2 (sep (punctuate comma fields))) <> rparen
  format (Sym { symName = name }) = format name

instance Format sym => Format (Pattern sym) where
  format (Project { projectFields = fields, projectStrict = True }) =
    lparen <> (nest 2 (sep (punctuate comma fields))) <> rparen
  format (Project { projectFields = fields, projectStrict = False }) =
    let
      withdots = (map format fields) ++ [format "..."]
    in
    lparen <> (nest 2 (sep (punctuate comma withdots))) <> rparen
  format (Construct { constructName = name, constructArgs = args,
                      constructStrict = True }) = parenList name args
  format (Construct { constructName = name, constructArgs = args,
                      constructStrict = False }) =
    parenList name ((map format args) ++ [format "..."])
  format (Typed { typedPat = pat, typedType = ty }) =
    hang (pat <+> colon) 2 ty
  format (As { asName = name, asPat = pat }) =
    format pat <+> format "as" <+> format name
  format (Name { nameSym = name }) = format name

instance Format sym => Format (Binding sym) where
  format (Binding { bindingName = name, bindingType = ty }) =
    hang (name <+> colon) 2 ty

instance Format sym => Format (Case sym) where
  format (Case { casePat = pat, caseBody = body }) =
    hang (pat <+> equals) 2 body

instance (Format sym, Format (con sym)) => Format (Entry con sym) where
  format (Named { namedName = name, namedVal = val }) =
    hang val 2 (format "as" <+> name)
  format (Unnamed e) = format e

instance Format sym => Show (Decl sym) where
  show = show . format

instance Format sym => Show (Exp sym) where
  show = show . format

arbitraryPos :: Pos
arbitraryPos = internal "arbitrary"

operatorChars :: [Char]
operatorChars = "!@$%^&*-+/~=<>?|"

idChars :: [Char]
idChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPGRSTUVWXYZ_"

numChars :: [Char]
numChars = "0123456789"

arbitraryNumber :: Gen String
arbitraryNumber = listOf1 (elements numChars)

arbitraryOperator :: Gen String
arbitraryOperator = listOf1 (elements operatorChars)

arbitraryName :: Gen String
arbitraryName =
  do
    first <- elements idChars
    rest <- listOf (elements (idChars ++ numChars))
    return (first : rest)
{-
arbitraryIdentifier :: Gen String
arbitraryIdentifier = oneof [ arbitraryOperator, arbitraryName ]
-}
arbitrarySymbol :: Gen String
arbitrarySymbol = oneof [ arbitraryNumber, arbitraryOperator, arbitraryName ]

arbitraryExp :: Int -> Gen (Exp String)
arbitraryExp size =
  let
    arbitraryAscribe :: Gen (Exp String)
    arbitraryAscribe =
      do
        valsize <- choose (0, size - 1)
        val <- arbitraryExp valsize
        ty <- arbitraryExp (size - valsize)
        return Ascribe { ascribeVal = val, ascribeType = ty,
                         ascribePos = arbitraryPos }

    arbitrarySeq :: Gen (Exp String)
    arbitrarySeq =
      let
        arbitraryExpList :: Int -> Gen ([Exp String])
        arbitraryExpList 0 = return []
        arbitraryExpList listsize =
          do
            thissize <- choose (0, listsize - 1)
            this <- arbitraryExp thissize
            rest <- arbitraryExpList (listsize - thissize - 1)
            return (this : rest)
      in do
        vals <- arbitraryExpList size
        return Seq { seqVals = vals, seqPos = arbitraryPos }

    arbitraryRecord :: Gen (Exp String)
    arbitraryRecord =
      let
        arbitraryUnnamedFields :: Int -> Gen [Entry Exp String]
        arbitraryUnnamedFields 0 = return []
        arbitraryUnnamedFields listsize =
          do
            thissize <- choose (0, listsize - 1)
            this <- arbitraryExp thissize
            rest <- arbitraryUnnamedFields (listsize - thissize - 1)
            return (Unnamed this : rest)

        arbitraryNamedFields :: Int -> Gen [Entry Exp String]
        arbitraryNamedFields 0 = return []
        arbitraryNamedFields listsize =
          do
            thissize <- choose (0, listsize - 1)
            sym <- arbitraryName
            this <- arbitraryExp thissize
            rest <- arbitraryNamedFields (listsize - thissize - 1)
            return (Named { namedName = sym, namedVal = this,
                            namedPos = arbitraryPos } : rest)
      in do
        fields <- oneof [ arbitraryUnnamedFields size,
                          arbitraryNamedFields size ]
        return Record { recFields = fields, recPos = arbitraryPos }

    arbitrarySym :: Gen (Exp String)
    arbitrarySym =
      do
        name <- arbitrarySymbol
        return Sym { symName = name, symPos = internal "arbitrary" }
  in case size of
    0 -> arbitrarySym
    1 -> oneof [
        arbitraryAscribe,
        arbitrarySym
      ]
    _ -> oneof [
        arbitraryAscribe,
        arbitrarySeq,
        arbitraryRecord,
        arbitrarySym
      ]

instance Arbitrary (Exp String) where
  arbitrary = sized arbitraryExp