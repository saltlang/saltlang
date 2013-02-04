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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- | The Abstract Syntax Tree, as yielded by a parser.  This is
-- rendered into surface syntax by Collect, which gathers all entries
-- into a table.
module Language.Argent.Surface.AST(
       ScopeClass(..),
       Decl(..),
       Compound(..),
       Exp(..),
       Binding(..)
       ) where

import Control.Applicative
import Data.Foldable
import Data.Hash
import Data.Monoid
import Data.Pos
import Data.Traversable
import Language.Argent.Surface.Common
import Prelude.Extras(Eq1(..), Ord1(..))
import Prelude.Extras.ExtraInstances()

-- | Declarations.  These represent static declarations inside a
-- scope.  Note: some of these can be built from others.
data Decl
       -- | The type of a symbol
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
       -- | The type of a symbol
       sym =
    -- | An ordinary expression.
    Exp !(Exp sym)
    -- | A declaration.
  | Decl !(Decl sym)

-- | Expressions.  These represent computed values of any type.
data Exp
       -- | The type of a symbol
       sym =
    -- | An expression that may contain declarations as well as
    -- expressions.
    Compound {
      -- | The body of the compound expression.
      compoundBody :: [Compound sym],
      -- | The position in source from which this arises.
      compoundPos :: !Pos
    }
    -- | Reference to a name.  Note: this is all handled with the
    -- Bound framework.
  | Sym {
      -- | The name being referenced.
      symName :: !sym,
      -- | The position in source from which this arises.
      symPos :: !Pos
    }

-- | A simple name binding.
data Binding
       -- | The type of a symbol
       sym =
  Binding {
    -- | The name being bound.
    bindingName :: !sym,
    -- | The type assigned to the bound name.
    bindingType :: Exp sym,
      -- | The position in source from which this arises.
    bindingPos :: !Pos
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

instance Eq1 Exp where
  (Sym { symName = name1 }) ==# (Sym { symName = name2 }) = name1 == name2
  (Compound { compoundBody = body1 }) ==#
    (Compound { compoundBody = body2 }) = body1 ==# body2
  _ ==# _ = False

instance Eq sym => Eq (Decl sym) where (==) = (==#)
instance Eq sym => Eq (Compound sym) where (==) = (==#)
instance Eq sym => Eq (Exp sym) where (==) = (==#)
instance Eq sym => Eq (Binding sym) where (==) = (==#)

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
  compare1 (Sym { symName = name1 })
           (Sym { symName = name2 }) =
    compare name1 name2

instance Ord1 Binding where
  compare1 (Binding { bindingName = name1, bindingType = ty1 })
           (Binding { bindingName = name2, bindingType = ty2 }) =
    case compare name1 name2 of
      EQ -> compare1 ty1 ty2
      out -> out

instance Ord sym => Ord (Decl sym) where compare = compare1
instance Ord sym => Ord (Compound sym) where compare = compare1
instance Ord sym => Ord (Exp sym) where compare = compare1
instance Ord sym => Ord (Binding sym) where compare = compare1

instance Eq1 Binding where
  (Binding { bindingName = name1, bindingType = ty1 }) ==#
    (Binding { bindingName = name2, bindingType = ty2 }) =
    (name1 == name2) && (ty1 ==# ty2)

instance Position (Decl sym) where
  pos (Scope { scopePos = p }) = p
  pos (Value { valuePos = p }) = p
  pos (Invariant { invPos = p }) = p

instance Position (Compound sym) where
  pos (Decl d) = pos d
  pos (Exp e) = pos e

instance Position (Exp sym) where
  pos (Compound { compoundPos = p }) = p
  pos (Sym { symPos = p }) = p

instance Position (Binding sym) where
  pos (Binding { bindingPos = p }) = p

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
  hash (Sym { symName = name }) = hashInt 2 `combine` hash name

instance Hashable sym => Hashable (Binding sym) where
  hash (Binding { bindingName = name, bindingType = ty }) =
    hash name `combine` hash ty

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
  fmap f e @ (Compound { compoundBody = body }) =
    e { compoundBody = fmap (fmap f) body }

instance Functor Binding where
  fmap f b @ (Binding { bindingName = name, bindingType = ty }) =
    b { bindingName = f name, bindingType = fmap f ty }

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
  foldMap f (Compound { compoundBody = body }) = foldMap (foldMap f) body

instance Foldable Binding where
  foldMap f (Binding { bindingName = name, bindingType = ty }) =
    f name `mappend` foldMap f ty

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
  traverse f e @ (Compound { compoundBody = body }) =
    (\body' -> e { compoundBody = body' }) <$> traverse (traverse f) body

instance Traversable Binding where
  traverse f b @ (Binding { bindingName = name, bindingType = ty }) =
    (\name' ty' -> b { bindingName = name', bindingType = ty' }) <$>
      f name <*> traverse f ty