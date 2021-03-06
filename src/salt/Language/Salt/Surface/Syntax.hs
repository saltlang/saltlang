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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             DeriveTraversable, DeriveFoldable, DeriveFunctor #-}

-- | Abstract Syntax structure.  This represents the surface language
-- in a more abstract form than is found in the AST structure.  In
-- this form, definitions have been gathered up into tables, and some
-- amount of processing has been done.  However, symbols have not been
-- resolved and inherited scopes have not been constructed.
module Language.Salt.Surface.Syntax(
       DefID,
       Ref(..),
       Surface(..),
       Component(..),
       Assoc(..),
       Fixity(..),
       Syntax(..),
       Truth(..),
       Proof(..),
       Scope(..),
       Builder(..),
       Def(..),
       Import(..),
       Compound(..),
       Pattern(..),
       Literal(..),
       Exp(..),
       Entry(..),
       Fields(..),
       Field(..),
       Case(..)
       ) where

import Control.Monad.Positions
import Control.Monad.State
import Control.Monad.Symbols
import Data.Array
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.List(sort)
import Data.PositionElement
import Data.ScopeID
import Data.Symbol
import Data.Word
import Language.Salt.Format
import Language.Salt.Surface.Common
import Prelude hiding (init, exp, Either(..))
import Text.Format hiding ((<$>))
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)

import qualified Data.HashMap.Strict as HashMap

-- | Unique ID for initializers.
newtype DefID = DefID { defID :: Word } deriving (Eq, Ord, Ix)

-- | A reference to a definition in a scope.
data Ref =
  Ref {
    -- | The symbol in the scope.
    refSymbol :: !Symbol,
    -- | The scope ID.
    refScopeID :: !ScopeID
  }
  deriving (Ord, Eq)

data Surface expty =
  Surface {
    -- | Table of all scopes in the program.
    surfScopes :: !(Array ScopeID (Scope expty)),
    -- | List of all components in the program.
    surfComponents :: ![Component]
  }
  deriving (Functor, Foldable, Traversable)

-- | A component.  Essentially, a scope that may or may not have an
-- expected definition.
data Component =
  Component {
    -- | The expected definition.
    compExpected :: !(Maybe Symbol),
    -- | The top-level scope for this component.
    compScope :: !ScopeID
  }

-- | A static scope.  Elements are split up by kind, into builder definitions,
-- syntax directives, truths, proofs, and regular definitions.
data Scope expty =
  Scope {
    -- | All the builders defined in this scope.
    scopeBuilders :: !(HashMap Symbol (Builder expty)),
    -- | The syntax directives for this scope.
    scopeSyntax :: !(HashMap Symbol (Syntax expty)),
    -- | The truth environment for this scope.  This contains all
    -- theorems, axioms, and invariants.
    scopeTruths :: !(HashMap Symbol (Truth expty)),
    -- | All concrete definitions in this scope.
    scopeDefs :: !(Array DefID (Def expty)),
    -- | A map from names to definition IDs.
    scopeNames :: !(Array Visibility (HashMap Symbol [DefID])),
    -- | Proofs given in this scope.
    scopeProofs :: ![Proof expty],
    -- | Imports in this scope.
    scopeImports :: ![Import expty],
    -- | The enclosing scope, if one exists.
    scopeEnclosing :: !(Maybe ScopeID)
  }
  deriving (Eq, Functor, Foldable, Traversable)

-- | Syntax information for a symbol.
data Syntax expty =
  Syntax {
    -- | Fixity (and associativity).
    syntaxFixity :: !Fixity,
    -- | Precedence relations.
    syntaxPrecs :: ![(Ordering, expty)],
    -- | The position in source from which this arises.
    syntaxPos :: !Position
  }
  deriving (Ord, Eq, Functor, Foldable, Traversable)

-- | Truths.  These are similar to declarations, except that they
-- affect the proof environment.  These include theorems and
-- invariants.
data Truth expty =
  Truth {
    -- | The class of the truth.
    truthKind :: !TruthKind,
    -- | The visibility of the truth.
    truthVisibility :: !Visibility,
    -- | The truth proposition.
    truthContent :: !expty,
    -- | A proof (may or may not be supplied).
    truthProof :: !(Maybe expty),
    -- | The position in source from which this arises.
    truthPos :: !Position
  }
  deriving (Functor, Foldable, Traversable)

-- | A builder definition.
data Builder expty =
  Builder {
    -- | The type of entity the builder represents.
    builderKind :: !BuilderKind,
    -- | The visibility of this definition.
    builderVisibility :: !Visibility,
    -- | The parameters of the builder entity.
    builderParams :: !(Fields expty),
    -- | The declared supertypes for this builder entity.
    builderSuperTypes :: ![expty],
    -- | The entities declared by the builder.
    builderContent :: !expty,
    -- | The position in source from which this arises.
    builderPos :: !Position
  }
  deriving (Functor, Foldable, Traversable)

-- | Directives.  These are static commands, like imports or proofs,
-- which influence a scope, but do not directly define anything.
data Proof expty =
    -- | A proof.  This is just a code block with the name of the
    -- theorem being proven.
    Proof {
      -- | The name of the theorem being proven.
      proofName :: !expty,
      -- | The body of the proof.
      proofBody :: !expty,
      -- | The position in source from which this arises.
      proofPos :: !Position
    }
    deriving (Functor, Foldable, Traversable)

-- | Value Definitions.
data Def expty =
  -- | Value definitions.  These are declarations coupled with
  -- values.  These include function declarations.
  Def {
    -- | The binding pattern.
    defPattern :: !(Pattern expty),
    -- | The value's initializer.
    defInit :: !(Maybe expty),
    -- | The position in source from which this arises.
    defPos :: !Position
  }
  deriving (Functor, Foldable, Traversable)

-- | An import.
data Import expty =
  Import {
    -- | The visibility of the truth.
    importVisibility :: !Visibility,
    -- | The name(s) to import.
    importExp :: !expty,
    -- | The position in source from which this arises.
    importPos :: !Position
  }
  deriving (Functor, Foldable, Traversable)

-- | Compound expression elements.  These are either "ordinary"
-- expressions, or declarations.
data Compound expty =
    -- | An ordinary expression.
    Exp { expVal :: !expty }
    -- | The position of a symbol declaration.
  | Decl { declSym :: !Symbol }
    -- | The position of an initializer.
  | Init { initId :: !DefID }
    deriving (Functor, Foldable, Traversable)

-- | A pattern, for pattern match expressions.
data Pattern expty =
    Option {
      -- | The option patterns
      optionPats :: ![Pattern expty],
      -- | The position in source from which this arises.
      optionPos :: !Position
    }
    -- | A deconstructor pattern.  Mirrors a call expression.
  | Deconstruct {
      -- | The name of the constructor.
      deconstructName :: !Symbol,
      -- | The arguments to the constructor.
      deconstructPat :: !(Pattern expty),
      -- | The position in source from which this arises.
      deconstructPos :: !Position
    }
    -- | A projection.  Mirrors a record expression.
  | Split {
      -- | The fields being projected.
      splitFields :: !(HashMap Symbol (Entry expty)),
      -- | Whether or not the binding is strict (ie. it omits some fields)
      splitStrict :: !Bool,
      -- | The position in source from which this arises.
      splitPos :: !Position
    }
    -- | A typed pattern.  Fixes the type of a pattern.
  | Typed {
      -- | The pattern whose type is being fixed.
      typedPat :: !(Pattern expty),
      -- | The type to which the pattern is fixed.
      typedType :: !expty,
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
      asPat :: !(Pattern expty),
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
  | Exact { exactLit :: !Literal }
    deriving (Functor, Foldable, Traversable)

-- | Expressions.  These represent computed values of any type.
data Exp refty =
    -- | An expression that may contain declarations as well as
    -- expressions.  This structure defines a dynamic scope.  Syntax
    -- directives and proofs are moved out-of-line, while truths,
    -- builders, and regular definitions remain in-line.
    Compound {
      -- | Scope containing definitions in this compound block.
      compoundScope :: !ScopeID,
      -- | The body of the compound block.
      compoundBody :: ![Compound (Exp refty)],
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
      absCases :: ![Case (Exp refty)],
      -- | The position in source from which this arises.
      absPos :: !Position
    }
    -- | Match statement.  Given a set of (possibly typed) patterns,
    -- find the meet of all types above the argument's type in the
    -- cases, then apply the first pattern for the meet type that
    -- matches the argument.
  | Match {
      -- | The argument to the match statement.
      matchVal :: (Exp refty),
      -- | The cases, in order.
      matchCases :: ![Case (Exp refty)],
      -- | The position in source from which this arises.
      matchPos :: !Position
    }
    -- | Ascribe expression.  Fixes the type of a given expression.
  | Ascribe {
      -- | The expression whose type is being set.
      ascribeVal :: (Exp refty),
      -- | The type.
      ascribeType :: (Exp refty),
      -- | The position in source from which this arises.
      ascribePos :: !Position
    }
    -- | A sequence of expressions.  The entire sequence represents a
    -- function call, possibly with inorder symbols.  This is
    -- re-parsed once the inorder symbols are known.
  | Seq {
      -- | The first expression.
      seqExps :: ![Exp refty],
      -- | The position in source from which this arises.
      seqPos :: !Position
    }
  | Call {
      callFunc :: !(Exp refty),
      callArgs :: !(HashMap Symbol (Exp refty)),
      callPos :: !Position
    }
  | RecordType {
      recordTypeFields :: !(Fields (Exp refty)),
      recordTypePos :: !Position
    }
    -- | A record literal.  Can represent a record type, or a record value.
  | Record {
      -- | The fields in this record expression.
      recordFields :: !(HashMap FieldName (Exp refty)),
      -- | The position in source from which this arises.
      recordPos :: !Position
    }
    -- | A tuple literal.
  | Tuple {
      -- | The fields in this tuple expression.
      tupleFields :: ![Exp refty],
      -- | The position in source from which this arises.
      tuplePos :: !Position
    }
    -- | A field expression.  Accesses the given field in a record.
    -- Note that for non-static functions, this implies an extra
    -- argument.
  | Project {
      -- | The inner expression
      projectVal :: (Exp refty),
      -- | The name of the field being accessed.
      projectFields :: ![FieldName],
      -- | The position in source from which this arises.
      projectPos :: !Position
    }
    -- | Reference to a name.  Note: this is all handled with the
    -- Bound framework.
  | Sym {
      -- | The name being referenced.
      symRef :: !refty,
      -- | The position in source from which this arises.
      symPos :: !Position
    }
    -- | A with expression.  Represents currying.
  | With {
      -- | The value to which some arguments are being applied.
      withVal :: (Exp refty),
      -- | The argument(s) to apply
      withArgs :: (Exp refty),
      -- | The position in source from which this arises.
      withPos :: !Position
    }
    -- | Where expression.  Constructs refinement types.
  | Where {
      -- | The value to which some arguments are being applied.
      whereVal :: (Exp refty),
      -- | The argument(s) to apply
      whereProp :: (Exp refty),
      -- | The position in source from which this arises.
      wherePos :: !Position
    }
    -- | Builder literal.
  | Anon {
      -- | The type of entity the builder represents.
      anonKind :: !BuilderKind,
      -- | The declared supertypes for this builder entity.
      anonSuperTypes :: ![Exp refty],
      -- | The parameters of the builder entity.
      anonParams :: !(Fields (Exp refty)),
      -- | The entities declared by the builder.
      anonContent :: !ScopeID,
      -- | The position in source from which this arises.
      anonPos :: !Position
    }
    -- | Number literal.
  | Literal { literalVal :: !Literal }
    deriving (Functor, Foldable, Traversable)

-- | An entry in a record field or call argument list.
data Entry expty =
  -- | A named field.  This sets a specific field or argument to a
  -- certain value.
  Entry {
    -- | The value to which the field or argument is set.
    entryPat :: !(Pattern expty),
    -- | The position in source from which this arises.
    entryPos :: !Position
  }
  deriving (Functor, Foldable, Traversable)

-- | Representation of fields.  This contains information for
-- interpreting record as well as tuple values.
data Fields expty =
  Fields {
    -- | Named bindings.
    fieldsBindings :: !(HashMap FieldName (Field expty)),
    -- | A mapping from field positions to names.
    fieldsOrder :: !(Array Word FieldName)
  }
  deriving (Functor, Foldable, Traversable)

-- | A field.
data Field expty =
  Field {
    -- | The value assigned to the bound name.
    fieldVal :: !expty,
    -- | The position in source from which this arises.
    fieldPos :: !Position
  }
  deriving (Functor, Foldable, Traversable)

-- | A case in a match statement or a function definition.
data Case expty =
  Case {
    -- | The pattern to match for this case.
    casePat :: !(Pattern expty),
    -- | The expression to execute for this case.
    caseBody :: !expty,
    -- | The position in source from which this arises.
    casePos :: !Position
  }
  deriving (Functor, Foldable, Traversable)

instance Enum DefID where
  succ = DefID . succ . defID
  pred = DefID . pred . defID
  toEnum = DefID . toEnum
  fromEnum = fromEnum . defID
  enumFromThen DefID { defID = n } = map DefID . enumFromThen n . defID
  enumFromTo DefID { defID = n } = map DefID . enumFromTo n . defID
  enumFromThenTo DefID { defID = n } DefID { defID = m } =
    map DefID . enumFromThenTo n m . defID

instance PositionElement (Builder expty) where
  position Builder { builderPos = pos } = pos

instance PositionElement (Def expty) where
  position Def { defPos = pos } = pos

instance PositionElement (Truth expty) where
  position Truth { truthPos = pos } = pos

instance PositionElement (Proof expty) where
  position Proof { proofPos = pos } = pos

instance PositionElement (Import expty) where
  position Import { importPos = pos } = pos

instance PositionElement (Syntax expty) where
  position Syntax { syntaxPos = pos } = pos

instance PositionElement (Pattern expty) where
  position Option { optionPos = pos } = pos
  position Deconstruct { deconstructPos = pos } = pos
  position Split { splitPos = pos } = pos
  position Typed { typedPos = pos } = pos
  position As { asPos = pos } = pos
  position Name { namePos = pos } = pos
  position Exact { exactLit = l } = position l

instance PositionElement (Exp refty) where
  position Compound { compoundPos = pos } = pos
  position Abs { absPos = pos } = pos
  position Match { matchPos = pos } = pos
  position Ascribe { ascribePos = pos } = pos
  position Seq { seqPos = pos } = pos
  position Call { callPos = pos } = pos
  position RecordType { recordTypePos = pos } = pos
  position Record { recordPos = pos } = pos
  position Tuple { tuplePos = pos } = pos
  position Project { projectPos = pos } = pos
  position Sym { symPos = pos } = pos
  position With { withPos = pos } = pos
  position Where { wherePos = pos } = pos
  position Anon { anonPos = pos } = pos
  position Literal { literalVal = l } = position l

instance PositionElement (Entry expty) where
  position Entry { entryPos = pos } = pos

instance PositionElement (Field expty) where
  position Field { fieldPos = pos } = pos

instance PositionElement (Case expty) where
  position Case { casePos = pos } = pos

instance Eq Component where
  Component { compExpected = expected1, compScope = scope1 } ==
    Component { compExpected = expected2, compScope = scope2 } =
      expected1 == expected2 && scope1 == scope2

instance Eq expty => Eq (Truth expty) where
  Truth { truthKind = kind1, truthVisibility = vis1,
          truthContent = content1, truthProof = proof1 } ==
    Truth { truthKind = kind2, truthVisibility = vis2,
            truthContent = content2, truthProof = proof2 } =
      kind1 == kind2 && vis1 == vis2 && content1 == content2 && proof1 == proof2

instance Eq expty => Eq (Builder expty) where
  Builder { builderKind = kind1, builderVisibility = vis1,
            builderParams = params1, builderSuperTypes = supers1,
            builderContent = content1 } ==
    Builder { builderKind = kind2, builderVisibility = vis2,
              builderParams = params2, builderSuperTypes = supers2,
              builderContent = content2 } =
      kind1 == kind2 && vis1 == vis2 && params1 == params2 &&
      supers1 == supers2 && content1 == content2

instance Eq expty => Eq (Proof expty) where
  Proof { proofName = name1, proofBody = body1 } ==
    Proof { proofName = name2, proofBody = body2 } =
      name1 == name2 && body1 == body2

instance Eq expty => Eq (Def expty) where
  Def { defPattern = pat1, defInit = init1 } ==
    Def { defPattern = pat2, defInit = init2 } =
      pat1 == pat2 && init1 == init2

instance Eq expty => Eq (Import expty) where
  Import { importVisibility = vis1, importExp = exp1 } ==
    Import { importVisibility = vis2, importExp = exp2 } =
      vis1 == vis2 && exp1 == exp2

instance Eq expty => Eq (Compound expty) where
  Exp { expVal = exp1 } == Exp { expVal = exp2 } = exp1 == exp2
  Decl { declSym = sym1 } == Decl { declSym = sym2 } = sym1 == sym2
  Init { initId = id1 } == Init { initId = id2 } = id1 == id2
  _ == _ = False

instance Eq expty => Eq (Pattern expty) where
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
  As { asName = name1, asPat = pat1 } ==
    As { asName = name2, asPat = pat2 } =
      name1 == name2 && pat1 == pat2
  Name { nameSym = name1 } == Name { nameSym = name2 } = name1 == name2
  Exact { exactLit = lit1 } == Exact { exactLit = lit2 } = lit1 == lit2
  _ == _ = False

instance Eq refty => Eq (Exp refty) where
  Compound { compoundBody = body1, compoundScope = scope1 } ==
    Compound { compoundBody = body2, compoundScope = scope2 } =
      scope1 == scope2 && body1 == body2
  Abs { absKind = kind1, absCases = cases1 } ==
    Abs { absKind = kind2, absCases = cases2 } =
      kind1 == kind2 && cases1 == cases2
  Match { matchVal = val1, matchCases = cases1 } ==
    Match { matchVal = val2, matchCases = cases2 } =
      val1 == val2 && cases1 == cases2
  Ascribe { ascribeVal = val1, ascribeType = ty1 } ==
    Ascribe { ascribeVal = val2, ascribeType = ty2 } =
      val1 == val2 && ty1 == ty2
  Call { callFunc = func1, callArgs = args1 } ==
    Call { callFunc = func2, callArgs = args2 } =
      func1 == func2 && args1 == args2
  Seq { seqExps = exps1 } == Seq { seqExps = exps2 } = exps1 == exps2
  RecordType { recordTypeFields = fields1 } ==
    RecordType { recordTypeFields = fields2 } =
      fields1 == fields2
  Record { recordFields = fields1 } == Record { recordFields = fields2 } =
    fields1 == fields2
  Tuple { tupleFields = fields1 } == Tuple { tupleFields = fields2 } =
    fields1 == fields2
  Project { projectVal = val1, projectFields = names1 } ==
    Project { projectVal = val2, projectFields = names2 } =
      names1 == names2 && val1 == val2
  Sym { symRef = sym1 } == Sym { symRef = sym2 } = sym1 == sym2
  With { withVal = val1, withArgs = args1 } ==
    With { withVal = val2, withArgs = args2 } =
      val1 == val2 && args1 == args2
  Where { whereVal = val1, whereProp = prop1 } ==
    Where { whereVal = val2, whereProp = prop2 } =
      val1 == val2 && prop1 == prop2
  Anon { anonKind = kind1, anonSuperTypes = supers1,
         anonParams = fields1, anonContent = content1 } ==
    Anon { anonKind = kind2, anonSuperTypes = supers2,
           anonParams = fields2, anonContent = content2 } =
      kind1 == kind2 && supers1 == supers2 &&
      fields1 == fields2 && content1 == content2
  Literal lit1 == Literal lit2 = lit1 == lit2
  _ == _ = False

instance Eq expty => Eq (Entry expty) where
  Entry { entryPat = pat1 } == Entry { entryPat = pat2 } = pat1 == pat2

instance Eq expty => Eq (Fields expty) where
  Fields { fieldsBindings = bindings1, fieldsOrder = order1 } ==
    Fields { fieldsBindings = bindings2, fieldsOrder = order2 } =
      bindings1 == bindings2 && order1 == order2

instance Eq expty => Eq (Field expty) where
  Field { fieldVal = val1 } == Field { fieldVal = val2 } = val1 == val2

instance Eq expty => Eq (Case expty) where
  Case { casePat = pat1, caseBody = body1 } ==
    Case { casePat = pat2, caseBody = body2 } =
      pat1 == pat2 && body1 == body2

instance Ord Component where
  compare Component { compExpected = expected1, compScope = scope1 }
          Component { compExpected = expected2, compScope = scope2 } =
    case compare expected1 expected2 of
      EQ -> compare scope1 scope2
      out -> out

instance Ord expty => Ord (Truth expty) where
  compare Truth { truthKind = kind1, truthVisibility = vis1,
                  truthContent = content1, truthProof = proof1 }
          Truth { truthKind = kind2, truthVisibility = vis2,
                  truthContent = content2, truthProof = proof2 } =
    case compare kind1 kind2 of
      EQ -> case compare vis1 vis2 of
        EQ -> case compare content1 content2 of
          EQ -> compare proof1 proof2
          out -> out
        out -> out
      out -> out

instance Ord expty => Ord (Scope expty) where
  compare Scope { scopeBuilders = builders1, scopeSyntax = syntax1,
                  scopeTruths = truths1, scopeDefs = defs1,
                  scopeProofs = proofs1, scopeImports = imports1,
                  scopeNames = names1, scopeEnclosing = enclosing1 }
          Scope { scopeBuilders = builders2, scopeSyntax = syntax2,
                  scopeTruths = truths2, scopeDefs = defs2,
                  scopeProofs = proofs2, scopeImports = imports2,
                  scopeNames = names2, scopeEnclosing = enclosing2 } =
    let
      mapfun (idx, tab) = (idx, sort (HashMap.toList tab))

      builderlist1 = sort (HashMap.toList builders1)
      builderlist2 = sort (HashMap.toList builders2)
      truthlist1 = sort (HashMap.toList truths1)
      truthlist2 = sort (HashMap.toList truths2)
      syntaxlist1 = sort (HashMap.toList syntax1)
      syntaxlist2 = sort (HashMap.toList syntax2)
      namelist1 = map mapfun (assocs names1)
      namelist2 = map mapfun (assocs names2)
    in
      case compare enclosing1 enclosing2 of
        EQ -> case compare builderlist1 builderlist2 of
          EQ -> case compare syntaxlist1 syntaxlist2 of
            EQ -> case compare truthlist1 truthlist2 of
              EQ -> case compare defs1 defs2 of
                EQ -> case compare proofs1 proofs2 of
                  EQ -> case compare imports1 imports2 of
                    EQ -> compare namelist1 namelist2
                    out -> out
                  out -> out
                out -> out
              out -> out
            out -> out
          out -> out
        out -> out

instance Ord expty => Ord (Builder expty) where
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

instance Ord expty => Ord (Proof expty) where
  compare Proof { proofName = name1, proofBody = body1 }
          Proof { proofName = name2, proofBody = body2 } =
    case compare name1 name2 of
      EQ -> compare body1 body2
      out -> out

instance Ord expty => Ord (Def expty) where
  compare Def { defPattern = pat1, defInit = init1 }
          Def { defPattern = pat2, defInit = init2 } =
    case compare pat1 pat2 of
      EQ -> compare init1 init2
      out -> out

instance Ord expty => Ord (Import expty) where
  compare Import { importVisibility = vis1, importExp = exp1 }
          Import { importVisibility = vis2, importExp = exp2 } =
    case compare vis1 vis2 of
      EQ -> compare exp1 exp2
      out -> out

instance Ord expty => Ord (Compound expty) where
  compare Exp { expVal = exp1 } Exp { expVal = exp2 } = compare exp1 exp2
  compare Exp {} _ = LT
  compare _ Exp {} = GT
  compare Decl { declSym = sym1 } Decl { declSym = sym2 } = compare sym1 sym2
  compare Decl {} _ = LT
  compare _ Decl {} = GT
  compare Init { initId = id1 } Init { initId = id2 } = compare id1 id2

instance Ord expty => Ord (Pattern expty) where
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
      fieldlist1 = sort (HashMap.toList fields1)
      fieldlist2 = sort (HashMap.toList fields2)
    in
      case compare strict1 strict2 of
        EQ -> compare fieldlist1 fieldlist2
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
  compare Name { nameSym = name1 } Name { nameSym = name2 } =
    compare name1 name2
  compare Name {} _ = LT
  compare _ Name {} = GT
  compare Exact { exactLit = lit1 } Exact { exactLit = lit2 } =
    compare lit1 lit2

instance Ord expty => Ord (Exp expty) where
  compare Compound { compoundBody = body1, compoundScope = scope1 }
          Compound { compoundBody = body2, compoundScope = scope2 } =
    case compare scope1 scope2 of
      EQ -> compare body1 body2
      out -> out
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
  compare Ascribe { ascribeVal = val1, ascribeType = ty1 }
          Ascribe { ascribeVal = val2, ascribeType = ty2 } =
    case compare val1 val2 of
      EQ -> compare ty1 ty2
      out -> out
  compare Ascribe {} _ = LT
  compare _ Ascribe {} = GT
  compare Seq { seqExps = exps1 } Seq { seqExps = exps2 } = compare exps1 exps2
  compare Seq {} _ = LT
  compare _ Seq {} = GT
  compare Call { callFunc = func1, callArgs = args1 }
          Call { callFunc = func2, callArgs = args2 } =
    let
      arglist1 = sort (HashMap.toList args1)
      arglist2 = sort (HashMap.toList args2)
    in case compare func1 func2 of
        EQ -> compare arglist1 arglist2
        out -> out
  compare Call {} _ = LT
  compare _ Call {} = GT
  compare RecordType { recordTypeFields = fields1 }
          RecordType { recordTypeFields = fields2 } =
    compare fields1 fields2
  compare RecordType {} _ = LT
  compare _ RecordType {} = GT
  compare Record { recordFields = fields1 } Record { recordFields = fields2 } =
    let
      fieldlist1 = sort (HashMap.toList fields1)
      fieldlist2 = sort (HashMap.toList fields2)
    in
      compare fieldlist1 fieldlist2
  compare Record {} _ = LT
  compare _ Record {} = GT
  compare Tuple { tupleFields = fields1 } Tuple { tupleFields = fields2 } =
    compare fields1 fields2
  compare Tuple {} _ = LT
  compare _ Tuple {} = GT
  compare Project { projectVal = val1, projectFields = names1 }
          Project { projectVal = val2, projectFields = names2 } =
    case compare names1 names2 of
      EQ -> compare val1 val2
      out -> out
  compare Project {} _ = LT
  compare _ Project {} = GT
  compare Sym { symRef = sym1 } Sym { symRef = sym2 } = compare sym1 sym2
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
                 anonParams = fields1, anonContent = content1 }
          Anon { anonKind = kind2, anonSuperTypes = supers2,
                 anonParams = fields2, anonContent = content2 } =
    case compare kind1 kind2 of
      EQ -> case compare supers1 supers2 of
        EQ -> case compare fields1 fields2 of
          EQ -> compare content1 content2
          out -> out
        out -> out
      out -> out
  compare Anon {} _ = LT
  compare _ Anon {} = GT
  compare Literal { literalVal = lit1 } Literal { literalVal = lit2 } =
    compare lit1 lit2

instance Ord expty => Ord (Entry expty) where
  compare Entry { entryPat = pat1 } Entry { entryPat = pat2 } =
    compare pat1 pat2

instance Ord expty => Ord (Fields expty) where
  compare Fields { fieldsBindings = bindings1, fieldsOrder = order1 }
          Fields { fieldsBindings = bindings2, fieldsOrder = order2 } =
    let
      binds1 = map (\sym -> (sym, HashMap.lookup sym bindings1)) (elems order1)
      binds2 = map (\sym -> (sym, HashMap.lookup sym bindings2)) (elems order2)
    in
      compare binds1 binds2

instance Ord expty => Ord (Field expty) where
  compare Field { fieldVal = val1 } Field { fieldVal = val2 } =
    compare val1 val2

instance Ord expty => Ord (Case expty) where
  compare Case { casePat = pat1, caseBody = body1 }
          Case { casePat = pat2, caseBody = body2 } =
    case compare pat1 pat2 of
      EQ -> compare body1 body2
      out -> out

instance Hashable DefID where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable Ref where
  hashWithSalt s Ref { refScopeID = scope, refSymbol = sym } =
    s `hashWithSalt` scope `hashWithSalt` sym

instance Hashable Component where
  hashWithSalt s Component { compExpected = expected, compScope = scope } =
    s `hashWithSalt` expected `hashWithSalt` scope

instance (Hashable expty, Ord expty) => Hashable (Syntax expty) where
  hashWithSalt s Syntax { syntaxFixity = fixity, syntaxPrecs = precs } =
    s `hashWithSalt` fixity `hashWithSalt` precs

instance (Hashable expty, Ord expty) => Hashable (Truth expty) where
  hashWithSalt s Truth { truthKind = kind, truthVisibility = vis,
                         truthContent = content, truthProof = proof } =
    s `hashWithSalt` kind `hashWithSalt` vis `hashWithSalt`
    content `hashWithSalt` proof

instance (Hashable expty, Ord expty) => Hashable (Scope expty) where
  hashWithSalt s Scope { scopeBuilders = builders, scopeSyntax = syntax,
                         scopeTruths = truths, scopeDefs = defs,
                         scopeProofs = proofs, scopeImports = imports,
                         scopeNames = names, scopeEnclosing = enclosing } =
    let
      mapfun (idx, tab) = (idx, sort (HashMap.toList tab))
      builderlist = sort (HashMap.toList builders)
      truthlist = sort (HashMap.toList truths)
      syntaxlist = sort (HashMap.toList syntax)
      namelist = map mapfun (assocs names)
    in
      s `hashWithSalt` builderlist `hashWithSalt`
      syntaxlist `hashWithSalt` truthlist `hashWithSalt`
      elems defs `hashWithSalt` proofs `hashWithSalt`
      imports `hashWithSalt` namelist `hashWithSalt` enclosing

instance (Hashable expty, Ord expty) => Hashable (Builder expty) where
  hashWithSalt s Builder { builderKind = kind, builderVisibility = vis,
                           builderParams = params, builderSuperTypes = supers,
                           builderContent = content } =
    s `hashWithSalt` kind `hashWithSalt` vis `hashWithSalt`
    params `hashWithSalt` supers `hashWithSalt` content

instance (Hashable expty, Ord expty) => Hashable (Proof expty) where
  hashWithSalt s Proof { proofName = sym, proofBody = body } =
    s `hashWithSalt` sym `hashWithSalt` body

instance (Hashable expty, Ord expty) => Hashable (Def expty) where
  hashWithSalt s Def { defPattern = pat, defInit = init } =
    s `hashWithSalt` pat `hashWithSalt` init

instance (Hashable expty, Ord expty) => Hashable (Import expty) where
  hashWithSalt s Import { importExp = exp } =
    s `hashWithSalt` exp

instance (Hashable expty, Ord expty) => Hashable (Compound expty) where
  hashWithSalt s Exp { expVal = exp } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` exp
  hashWithSalt s Decl { declSym = sym } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` sym
  hashWithSalt s Init { initId = defid } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` defid

instance (Hashable expty, Ord expty) => Hashable (Pattern expty) where
  hashWithSalt s Option { optionPats = pats } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` pats
  hashWithSalt s Deconstruct { deconstructName = sym, deconstructPat = pat } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` sym `hashWithSalt` pat
  hashWithSalt s Split { splitFields = fields, splitStrict = strict } =
    let
      fieldlist = sort (HashMap.toList fields)
    in
      s `hashWithSalt` (2 :: Int) `hashWithSalt` fieldlist `hashWithSalt` strict
  hashWithSalt s Typed { typedPat = pat, typedType = ty } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` pat `hashWithSalt` ty
  hashWithSalt s As { asName = sym, asPat = pat } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` sym `hashWithSalt` pat
  hashWithSalt s Name { nameSym = sym } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` sym
  hashWithSalt s Exact { exactLit = lit } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` lit

instance (Hashable refty, Ord refty) => Hashable (Exp refty) where
  hashWithSalt s Compound { compoundBody = body, compoundScope = scope } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` scope `hashWithSalt` body
  hashWithSalt s Abs { absKind = kind, absCases = cases } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` kind `hashWithSalt` cases
  hashWithSalt s Match { matchVal = val, matchCases = cases } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` val `hashWithSalt` cases
  hashWithSalt s Ascribe { ascribeVal = val, ascribeType = ty } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` val `hashWithSalt` ty
  hashWithSalt s Seq { seqExps = exps } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` exps
  hashWithSalt s Call { callFunc = func, callArgs = args } =
    let
      arglist = sort (HashMap.toList args)
    in
      s `hashWithSalt` (6 :: Int) `hashWithSalt` func `hashWithSalt` arglist
  hashWithSalt s Record { recordFields = fields } =
    let
      fieldlist = sort (HashMap.toList fields)
    in
      s `hashWithSalt` (7 :: Int) `hashWithSalt` fieldlist
  hashWithSalt s RecordType { recordTypeFields = fields } =
    s `hashWithSalt` (8 :: Int) `hashWithSalt` fields
  hashWithSalt s Tuple { tupleFields = fields } =
    s `hashWithSalt` (9 :: Int) `hashWithSalt` fields
  hashWithSalt s Project { projectVal = val, projectFields = sym } =
    s `hashWithSalt` (10 :: Int) `hashWithSalt` sym `hashWithSalt` val
  hashWithSalt s Sym { symRef = sym } =
    s `hashWithSalt` (11 :: Int) `hashWithSalt` sym
  hashWithSalt s With { withVal = val, withArgs = args } =
    s `hashWithSalt` (12 :: Int) `hashWithSalt` val `hashWithSalt` args
  hashWithSalt s Where { whereVal = val, whereProp = prop } =
    s `hashWithSalt` (13 :: Int) `hashWithSalt` val `hashWithSalt` prop
  hashWithSalt s Anon { anonKind = cls, anonParams = params,
                        anonSuperTypes = supers, anonContent = body } =
    s `hashWithSalt` (14 :: Int) `hashWithSalt` cls `hashWithSalt`
    params `hashWithSalt` supers `hashWithSalt` body
  hashWithSalt s (Literal lit) =
    s `hashWithSalt` (15 :: Int) `hashWithSalt` lit

instance (Hashable expty, Ord expty) => Hashable (Entry expty) where
  hashWithSalt s Entry { entryPat = pat } = s `hashWithSalt` pat

instance (Hashable expty, Ord expty) => Hashable (Fields expty) where
  hashWithSalt s Fields { fieldsBindings = bindings, fieldsOrder = order } =
    let
      binds = map (\sym -> (sym, HashMap.lookup sym bindings)) (elems order)
    in
      s `hashWithSalt` binds

instance (Hashable expty, Ord expty) => Hashable (Field expty) where
  hashWithSalt s = hashWithSalt s . fieldVal

instance (Hashable expty, Ord expty) => Hashable (Case expty) where
  hashWithSalt s Case { casePat = pat, caseBody = body } =
    s `hashWithSalt` pat `hashWithSalt` body

{-
precDot :: MonadSymbols m => (Ordering, Exp) -> StateT Word m (Doc, String)
precDot (ord, exp) =
  let
    orddoc = case ord of
      LT -> string "<"
      EQ -> string "=="
      GT -> string ">"
  in do
    nodeid <- getNodeID
    (expnode, expname) <- expDot exp
    return (expnode <$>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Prec | " <>
                               orddoc <> string " | " <>
                               string "<exp> exp\"") <$>
                      string "shape = \"record\"") <>
            dquoted (string nodeid <> string ":exp") <>
            string " -> " <> string expname, nodeid)

syntaxDot :: MonadSymbols m => Syntax -> StateT Word m (Doc, String)
syntaxDot Syntax { syntaxFixity = fixity, syntaxPrecs = precs } =
  let
    elemEdge nodeid (_, elemname) =
      dquoted (string nodeid) <> string ":precs" <>
      string " -> " <> dquoted (string elemname)
  in do
    nodeid <- getNodeID
    precdocs <- mapM precDot precs
    return (vcat (map fst precdocs) <$>
            dquoted (string nodeid) <+>
            brackets (string "label = " <>
                      dquoted (string "Syntax | " <>
                               format fixity <> string " | " <>
                               string "<precs> precs\"") <$>
                      string "shape = \"record\"") <>
            dquoted (string nodeid <> string ":value") <>
            char ';' <$> vcat (map (elemEdge nodeid) precdocs), nodeid)
-}

instance Format DefID where
  format = format . fromEnum

instance Monad m => FormatM m DefID where
  formatM = return . format

instance (MonadSymbols m) => FormatM m Ref where
  formatM Ref { refSymbol = sym, refScopeID = scopeid } =
    do
      symdoc <- formatM sym
      return $! compoundApplyDoc (string "Ref")
                                 [(string "symbol", symdoc),
                                  (string "scope", format scopeid)]

instance (MonadSymbols m, MonadPositions m, FormatM m expty) =>
         FormatM m (Syntax expty) where
  formatM Syntax { syntaxFixity = fixity, syntaxPrecs = precs } =
    let
      formatPrec (ord, exp) =
        let
          orddoc = case ord of
            LT -> string "<"
            EQ -> string "=="
            GT -> string ">"
        in do
          expdoc <- formatM exp
          return $! compoundApplyDoc (string "Prec") [(string "ord", orddoc),
                                                      (string "name", expdoc)]
    in do
      precdocs <- mapM formatPrec precs
      return $! compoundApplyDoc (string "Syntax")
                               [(string "fixity", format fixity),
                                (string "scope", listDoc precdocs)]

instance (MonadSymbols m, MonadPositions m, FormatM m expty) =>
         FormatM m (Truth expty) where
  formatM Truth { truthVisibility = vis, truthContent = content,
                  truthKind = kind, truthProof = Nothing, truthPos = pos } =
    do
      posdoc <- formatM pos
      contentdoc <- formatM content
      return (compoundApplyDoc (string "Truth")
                             [(string "visibility", format vis),
                              (string "kind", format kind),
                              (string "pos", posdoc),
                              (string "content", contentdoc)])
  formatM Truth { truthVisibility = vis, truthContent = content,
                  truthKind = kind, truthProof = Just proof, truthPos = pos } =
    do
      posdoc <- formatM pos
      contentdoc <- formatM content
      proofdoc <- formatM proof
      return (compoundApplyDoc (string "Truth")
                             [(string "visibility", format vis),
                              (string "kind", format kind),
                              (string "pos", posdoc),
                              (string "proof", proofdoc),
                              (string "content", contentdoc)])

formatMap :: (MonadSymbols m, MonadPositions m, FormatM m key, FormatM m val) =>
             HashMap key val -> m Doc
formatMap hashmap =
  let
    formatEntry (sym, ent) =
      do
        symdoc <- formatM sym
        entdoc <- formatM ent
        return $! tupleDoc [symdoc, entdoc]
  in do
    entrydocs <- mapM formatEntry (HashMap.toList hashmap)
    return $! listDoc entrydocs

formatListMap :: (MonadSymbols m, MonadPositions m,
                  FormatM m key, FormatM m val) =>
                 HashMap key [val] -> m Doc
formatListMap hashmap =
  let
    formatEntry (sym, ent) =
      do
        symdoc <- formatM sym
        entdoc <- mapM formatM ent
        return $! tupleDoc [symdoc, listDoc entdoc]
  in do
    entrydocs <- mapM formatEntry (HashMap.toList hashmap)
    return $! listDoc entrydocs

formatNames :: (MonadSymbols m, MonadPositions m) =>
               Array Visibility (HashMap Symbol [DefID]) -> m Doc
formatNames arr =
  do
    hiddendocs <- formatListMap (arr ! Hidden)
    privatedocs <- formatListMap (arr ! Private)
    protecteddocs <- formatListMap (arr ! Protected)
    publicdocs <- formatListMap (arr ! Public)
    return $! compoundApplyDoc (string "Names")
                             [(string "hidden", hiddendocs),
                              (string "private", privatedocs),
                              (string "protected", protecteddocs),
                              (string "public", publicdocs)]

instance (MonadSymbols m, MonadPositions m) => FormatM m Component where
  formatM Component { compExpected = Just expected, compScope = scope } =
    do
      expecteddoc <- formatM expected
      return $ compoundApplyDoc (string "Component")
                              [(string "expected", expecteddoc),
                               (string "scope", format scope)]
  formatM Component { compExpected = Nothing, compScope = scope } =
      return $ compoundApplyDoc (string "Component") [(string "scope",
                                                       format scope)]

instance (MonadSymbols m, MonadPositions m, FormatM m expty) =>
         FormatM m (Scope expty) where
  formatM Scope { scopeBuilders = builders, scopeSyntax = syntax,
                  scopeTruths = truths, scopeDefs = defs,
                  scopeProofs = proofs, scopeNames = names,
                  scopeEnclosing = enclosing } =
    let
      mapfun (idx, def) =
        do
          defdoc <- formatM def
          return $! tupleDoc [format idx, defdoc]
    in do
      buildersdoc <- formatMap builders
      syntaxdoc <- formatMap syntax
      truthsdoc <- formatMap truths
      namesdoc <- formatNames names
      defsdoc <- mapM mapfun (assocs defs)
      proofsdoc <- mapM formatM proofs
      case enclosing of
        Just enclosing' ->
          return $! compoundApplyDoc (string "Scope")
                                     [(string "builders", buildersdoc),
                                      (string "syntax", syntaxdoc),
                                      (string "truths", truthsdoc),
                                      (string "defs", listDoc defsdoc),
                                      (string "names", namesdoc),
                                      (string "proofs", listDoc proofsdoc),
                                      (string "enclosing", format enclosing')]
        Nothing ->
          return $! compoundApplyDoc (string "Scope")
                                     [(string "builders", buildersdoc),
                                      (string "syntax", syntaxdoc),
                                      (string "truths", truthsdoc),
                                      (string "defs", listDoc defsdoc),
                                      (string "names", namesdoc),
                                      (string "proofs", listDoc proofsdoc)]

instance (MonadSymbols m, MonadPositions m, FormatM m expty) =>
         FormatM m (Builder expty) where
  formatM Builder { builderVisibility = vis, builderKind = cls,
                    builderSuperTypes = supers, builderParams = params,
                    builderContent = body, builderPos = pos } =
    do
      posdoc <- formatM pos
      superdocs <- mapM formatM supers
      paramdocs <- formatM params
      bodydoc <- formatM body
      return (compoundApplyDoc (string "Builder")
                             [(string "visibility", format vis),
                              (string "pos", posdoc),
                              (string "kind", format cls),
                              (string "params", paramdocs),
                              (string "supers", listDoc superdocs),
                              (string "body", bodydoc)])

instance (MonadSymbols m, MonadPositions m, FormatM m expty) =>
         FormatM m (Proof expty) where
  formatM Proof { proofName = exp, proofBody = body, proofPos = pos } =
    do
      namedoc <- formatM exp
      posdoc <- formatM pos
      bodydoc <- formatM body
      return (compoundApplyDoc (string "Proof")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "body", bodydoc)])

instance (MonadSymbols m, MonadPositions m, FormatM m expty) =>
         FormatM m (Def expty) where
  formatM Def { defPattern = pat, defInit = Just init, defPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      initdoc <- formatM init
      return (compoundApplyDoc (string "Group")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "init", initdoc)])
  formatM Def { defPattern = pat, defInit = Nothing, defPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (compoundApplyDoc (string "Group")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc)])

instance (MonadSymbols m, MonadPositions m, FormatM m expty) =>
         FormatM m (Import expty) where
  formatM Import { importVisibility = vis, importExp = exp, importPos = pos } =
    do
      expdoc <- formatM exp
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Import")
                             [(string "visibility", format vis),
                              (string "exp", expdoc),
                              (string "pos", posdoc)])

instance (MonadPositions m, MonadSymbols m, FormatM m expty) =>
         FormatM m (Compound expty) where
  formatM Exp { expVal = e } = formatM e
  formatM Decl { declSym = sym } =
    do
      symdoc <- formatM sym
      return $! string "declaration of" <> symdoc
  formatM Init { initId = defid } =
    return $! string "declaration of" <> format defid

instance (MonadPositions m, MonadSymbols m, FormatM m expty) =>
         FormatM m (Pattern expty) where
  formatM Option { optionPats = pats, optionPos = pos } =
    do
      posdoc <- formatM pos
      patsdoc <- mapM formatM pats
      return (compoundApplyDoc (string "Options")
                             [(string "pos", posdoc),
                              (string "patterns", listDoc patsdoc)])
  formatM Deconstruct { deconstructName = sym, deconstructPat = pat,
                        deconstructPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (compoundApplyDoc (string "Deconstruct")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "pat", patdoc)])
  formatM Split { splitFields = fields, splitStrict = True, splitPos = pos } =
    do
      fieldsdoc <- formatMap fields
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Split")
                             [(string "pos", posdoc),
                              (string "strict", string "true"),
                              (string "fields", fieldsdoc)])
  formatM Split { splitFields = fields, splitStrict = False, splitPos = pos } =
    do
      fieldsdoc <- formatMap fields
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Split")
                             [(string "pos", posdoc),
                              (string "strict", string "false"),
                              (string "fields", fieldsdoc)])
  formatM Typed { typedPat = pat, typedType = ty, typedPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      tydoc <- formatM ty
      return (compoundApplyDoc (string "Typed")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "type", tydoc)])
  formatM As { asName = sym, asPat = pat, asPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (compoundApplyDoc (string "As")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "pat", patdoc)])
  formatM Name { nameSym = sym, namePos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Name")
                             [(string "name", namedoc),
                              (string "pos", posdoc)])
  formatM (Exact e) = formatM e

instance (MonadPositions m, MonadSymbols m, FormatM m refty) =>
         FormatM m (Exp refty) where
  formatM Compound { compoundBody = body, compoundScope = scope,
                     compoundPos = pos } =
    do
      posdoc <- formatM pos
      bodydoc <- mapM formatM body
      return (compoundApplyDoc (string "Compound")
                             [(string "pos", posdoc),
                              (string "scope", format scope),
                              (string "body", listDoc bodydoc)])
  formatM Abs { absKind = kind, absCases = cases, absPos = pos } =
    do
      posdoc <- formatM pos
      casedocs <- mapM formatM cases
      return (compoundApplyDoc (string "Abs")
                             [(string "pos", posdoc),
                              (string "kind", format kind),
                              (string "cases", listDoc casedocs)])
  formatM Match { matchVal = val, matchCases = cases, matchPos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      casedocs <- mapM formatM cases
      return (compoundApplyDoc (string "Match")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "cases", listDoc casedocs)])
  formatM Ascribe { ascribeVal = val, ascribeType = ty, ascribePos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      tydoc <- formatM ty
      return (compoundApplyDoc (string "Ascribe")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "type", tydoc)])
  formatM Seq { seqExps = exps, seqPos = pos } =
    do
      posdoc <- formatM pos
      expdocs <- mapM formatM exps
      return (compoundApplyDoc (string "Seq")
                             [(string "pos", posdoc),
                              (string "exps", listDoc expdocs)])
  formatM Call { callFunc = func, callArgs = args, callPos = pos } =
    do
      posdoc <- formatM pos
      funcdoc <- formatM func
      argdocs <- formatMap args
      return (compoundApplyDoc (string "Call")
                               [(string "pos", posdoc),
                                (string "func", funcdoc),
                                (string "args", argdocs)])
  formatM Record { recordFields = fields, recordPos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- formatMap fields
      return (compoundApplyDoc (string "Record")
                             [(string "pos", posdoc),
                              (string "fields", fielddocs)])
  formatM RecordType { recordTypeFields = fields,
                       recordTypePos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- formatM fields
      return (compoundApplyDoc (string "RecordType")
                             [(string "pos", posdoc),
                              (string "fields", fielddocs)])
  formatM Tuple { tupleFields = fields, tuplePos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- mapM formatM fields
      return (compoundApplyDoc (string "Tuple")
                             [(string "pos", posdoc),
                              (string "fields", listDoc fielddocs)])
  formatM Project { projectVal = val, projectFields = fields,
                    projectPos = pos } =
    do
      fielddocs <- mapM formatM fields
      posdoc <- formatM pos
      valdoc <- formatM val
      return (compoundApplyDoc (string "Project")
                             [(string "fields", listDoc fielddocs),
                              (string "pos", posdoc),
                              (string "value", valdoc)])
  formatM Sym { symRef = sym, symPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      return (compoundApplyDoc (string "Sym")
                             [(string "name", namedoc),
                              (string "pos", posdoc)])
  formatM With { withVal = val, withArgs = args, withPos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      argdocs <- formatM args
      return (compoundApplyDoc (string "With")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "arg", argdocs)])
  formatM Where { whereVal = val, whereProp = prop, wherePos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      propdoc <- formatM prop
      return (compoundApplyDoc (string "Where")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "prop", propdoc)])
  formatM Anon { anonKind = cls, anonParams = params, anonContent = body,
                 anonSuperTypes = supers, anonPos = pos } =

    do
      posdoc <- formatM pos
      superdocs <- mapM formatM supers
      paramdocs <- formatM params
      return (compoundApplyDoc (string "Anon")
                             [(string "pos", posdoc),
                              (string "kind", format cls),
                              (string "params", paramdocs),
                              (string "supers", listDoc superdocs),
                              (string "body", format body)])
  formatM Literal { literalVal = l } = formatM l

instance (MonadPositions m, MonadSymbols m, FormatM m expty) =>
         FormatM m (Entry expty) where
  formatM Entry { entryPat = pat, entryPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (compoundApplyDoc (string "Entry")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc)])

instance (MonadPositions m, MonadSymbols m, FormatM m expty) =>
         FormatM m (Fields expty) where
  formatM Fields { fieldsBindings = bindings, fieldsOrder = order } =
    do
      bindingsdoc <- formatMap bindings
      orderdoc <- mapM formatM (elems order)
      return (compoundApplyDoc (string "Fields")
                             [(string "bindings", bindingsdoc),
                              (string "value", listDoc orderdoc)])

instance (MonadPositions m, MonadSymbols m, FormatM m expty) =>
         FormatM m (Field expty) where
  formatM Field { fieldVal = val, fieldPos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      return (compoundApplyDoc (string "Field")
                             [(string "pos", posdoc),
                              (string "value", valdoc)])

instance (MonadPositions m, MonadSymbols m, FormatM m expty) =>
         FormatM m (Case expty) where
  formatM Case { casePat = pat, caseBody = body, casePos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      bodydoc <- formatM body
      return (compoundApplyDoc (string "Case")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "body", bodydoc)])

precPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [NodeG [] tag text] expty) =>
              PU [NodeG [] tag text] (Ordering, expty)
precPickler = xpElem (gxFromString "prec")
                     (xpAttr (gxFromString "order") xpPrim) xpickle

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Syntax expty) where
  xpickle =
    xpWrap (\(fixity, (precs, pos)) -> Syntax { syntaxFixity = fixity,
                                                syntaxPrecs = precs,
                                                syntaxPos = pos },
            \Syntax { syntaxFixity = fixity, syntaxPrecs = precs,
                      syntaxPos = pos } -> (fixity, (precs, pos)))
           (xpElem (gxFromString "Syntax") xpickle
                   (xpPair (xpElemNodes (gxFromString "precs")
                                        (xpList precPickler))
                           (xpElemNodes (gxFromString "pos") xpickle)))

mapPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler (Attributes tag text) key,
               XmlPickler [NodeG [] tag text] val,
               Hashable key, Eq key) =>
              PU [NodeG [] tag text] (HashMap key val)
mapPickler = xpWrap (HashMap.fromList, HashMap.toList)
                    (xpElemNodes (gxFromString "Map")
                                 (xpList (xpElem (gxFromString "entry")
                                                 xpickle xpickle)))

listMapPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text,
                   XmlPickler (Attributes tag text) key,
                   XmlPickler [NodeG [] tag text] val,
                   Hashable key, Eq key) =>
                  PU [NodeG [] tag text] (HashMap key [val])
listMapPickler = xpWrap (HashMap.fromList, HashMap.toList)
                        (xpElemNodes (gxFromString "Map")
                                     (xpList (xpElem (gxFromString "entry")
                                                     xpickle (xpList xpickle))))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] DefID  where
  xpickle = xpWrap (toEnum, fromEnum)
                   (xpElemAttrs (gxFromString "DefID")
                                (xpAttr (gxFromString "id") xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [(tag, text)] DefID  where
  xpickle = xpWrap (toEnum, fromEnum) (xpAttr (gxFromString "def-id") xpPrim)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Truth expty) where
  xpickle =
    xpWrap (\((vis, kind), (body, proof, pos)) ->
             Truth { truthVisibility = vis, truthContent = body,
                     truthKind = kind, truthProof = proof, truthPos = pos },
            \Truth { truthVisibility = vis, truthKind = kind,
                    truthContent = body, truthProof = proof, truthPos = pos } ->
              ((vis, kind), (body, proof, pos)))
           (xpElem (gxFromString "Truth") (xpPair xpickle xpickle)
                   (xpTriple (xpElemNodes (gxFromString "type") xpickle)
                             (xpOption (xpElemNodes (gxFromString "proof")
                                                    xpickle))
                             (xpElemNodes (gxFromString "pos") xpickle)))

namesPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] (Array Visibility
                                              (HashMap Symbol [DefID]))
namesPickler =
  xpWrap (\(hiddens, privates, protecteds, publics) ->
           listArray (Hidden, Public) [hiddens, privates, protecteds, publics],
          \arr -> (arr ! Hidden, arr ! Private, arr ! Protected, arr ! Public))
         (xpElemNodes (gxFromString "defs")
                      (xp4Tuple (xpElemNodes (gxFromString "hidden")
                                             listMapPickler)
                                (xpElemNodes (gxFromString "private")
                                             listMapPickler)
                                (xpElemNodes (gxFromString "protected")
                                             listMapPickler)
                                (xpElemNodes (gxFromString "public")
                                             listMapPickler)))

defsPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [NodeG [] tag text] expty) =>
               PU [NodeG [] tag text] (Array DefID (Def expty))
defsPickler =
  let
    defPickler = xpElem (gxFromString "def") xpickle xpickle
  in
    xpWrap (\entries -> array (toEnum 0, toEnum (length entries - 1)) entries,
            assocs)
           (xpElemNodes (gxFromString "defs") (xpList defPickler))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Component where
  xpickle =
    xpWrap (\(expected, scope) -> Component { compExpected = expected,
                                              compScope = scope },
            \Component { compExpected = expected,
                         compScope = scope } -> (expected, scope))
           (xpElem (gxFromString "Component") (xpOption xpickle) xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Scope expty) where
  xpickle =
    xpWrap (\((builders, syntax, truths, enclosing),
              (defs, names, proofs, imports)) ->
             Scope { scopeBuilders = builders, scopeSyntax = syntax,
                     scopeTruths = truths, scopeNames = names,
                     scopeProofs = proofs, scopeImports = imports,
                     scopeDefs = defs, scopeEnclosing = enclosing },
            \Scope { scopeBuilders = builders, scopeSyntax = syntax,
                     scopeTruths = truths, scopeNames = names,
                     scopeProofs = proofs, scopeImports = imports,
                     scopeDefs = defs, scopeEnclosing = enclosing } ->
            ((builders, syntax, truths, enclosing),
             (defs, names, proofs, imports)))
           (xpElemNodes (gxFromString "Scope")
                        (xpPair (xp4Tuple (xpElemNodes (gxFromString "builders")
                                                       mapPickler)
                                          (xpElemNodes (gxFromString "syntax")
                                                       mapPickler)
                                          (xpElemNodes (gxFromString "truths")
                                                       mapPickler)
                                          (xpOption (xpElemAttrs
                                                      (gxFromString "enclosing")
                                                      xpickle)))
                                (xp4Tuple (xpElemNodes (gxFromString "defs")
                                                       defsPickler)
                                          (xpElemNodes (gxFromString "names")
                                                       namesPickler)
                                          (xpElemNodes (gxFromString "proofs")
                                                       (xpList xpickle))
                                          (xpElemNodes (gxFromString "imports")
                                                       (xpList xpickle)))))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Builder expty) where
  xpickle =
    xpWrap (\((vis, kind), (params, supers, body, pos)) ->
             Builder { builderVisibility = vis, builderKind = kind,
                       builderParams = params, builderSuperTypes = supers,
                       builderContent = body, builderPos = pos },
            \Builder { builderVisibility = vis, builderKind = kind,
                       builderParams = params, builderSuperTypes = supers,
                       builderContent = body, builderPos = pos } ->
            ((vis, kind), (params, supers, body, pos)))
           (xpElem (gxFromString "Builder") (xpPair xpickle xpickle)
                   (xp4Tuple (xpElemNodes (gxFromString "params") xpickle)
                             (xpElemNodes (gxFromString "supers") xpickle)
                             (xpElemNodes (gxFromString "body") xpickle)
                             (xpElemNodes (gxFromString "pos") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Proof expty) where
  xpickle =
    xpWrap (\(pname, body, pos) -> Proof { proofName = pname,
                                           proofBody = body,
                                           proofPos = pos },
            \Proof { proofName = pname, proofBody = body,
                     proofPos = pos } -> (pname, body, pos))
           (xpElemNodes (gxFromString "Proof")
                        (xpTriple (xpElemNodes (gxFromString "name") xpickle)
                                  (xpElemNodes (gxFromString "type") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Def expty) where
  xpickle =
    xpWrap (\(pat, init, pos) -> Def { defPattern = pat, defInit = init,
                                       defPos = pos },
            \Def { defPattern = pat, defInit = init,
                   defPos = pos } -> (pat, init, pos))
           (xpElemNodes (gxFromString "Def")
                        (xpTriple (xpElemNodes (gxFromString "pattern") xpickle)
                                  (xpOption (xpElemNodes (gxFromString "init")
                                                         xpickle))
                                  (xpElemNodes (gxFromString "pos") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Import expty) where
  xpickle =
    xpWrap (\(vis, (exp, pos)) -> Import { importVisibility = vis,
                                           importExp = exp,
                                           importPos = pos },
            \Import { importVisibility = vis, importExp = exp,
                      importPos = pos } -> (vis, (exp, pos)))
           (xpElem (gxFromString "Import") xpickle
                   (xpPair (xpElemNodes (gxFromString "name") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

expPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler [NodeG [] tag text] expty) =>
              PU [NodeG [] tag text] (Compound expty)
expPickler =
  let
    revfunc Exp { expVal = e } = e
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Exp, revfunc) (xpElemNodes (gxFromString "Exp") xpickle)

elementPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text,
                   XmlPickler [NodeG [] tag text] expty) =>
                  PU [NodeG [] tag text] (Compound expty)
elementPickler =
  let
    revfunc Decl { declSym = sym } = sym
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Decl, revfunc) (xpElemNodes (gxFromString "Decl") xpickle)

initPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [NodeG [] tag text] expty) =>
               PU [NodeG [] tag text] (Compound expty)
initPickler =
  let
    revfunc Init { initId = defid } = defid
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Init, revfunc) (xpElemAttrs (gxFromString "Init") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Compound expty) where
  xpickle =
    let
      picker Exp {} = 0
      picker Decl {} = 1
      picker Init {} = 2
    in
      xpAlt picker [expPickler, elementPickler, initPickler]

optionPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text,
                  XmlPickler [NodeG [] tag text] expty) =>
                 PU [NodeG [] tag text] (Pattern expty)
optionPickler =
  let
    revfunc Option { optionPats = pats, optionPos = pos } = (pats, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pats, pos) -> Option { optionPats = pats,
                                     optionPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Option")
                        (xpPair (xpElemNodes (gxFromString "patterns") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

deconstructPickler :: (GenericXMLString tag, Show tag,
                       GenericXMLString text, Show text,
                       XmlPickler [NodeG [] tag text] expty) =>
                      PU [NodeG [] tag text] (Pattern expty)
deconstructPickler =
  let
    revfunc Deconstruct { deconstructName = sym, deconstructPat = pat,
                          deconstructPos = pos } =
      (sym, (pat, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, (pat, pos)) -> Deconstruct { deconstructName = sym,
                                                deconstructPat = pat,
                                                deconstructPos = pos },
            revfunc)
           (xpElem (gxFromString "Deconstruct") xpickle
                   (xpPair (xpElemNodes (gxFromString "pattern") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

splitPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] expty) =>
                PU [NodeG [] tag text] (Pattern expty)
splitPickler =
  let
    revfunc Split { splitStrict = strict, splitFields = fields,
                    splitPos = pos } =
      (strict, (fields, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(strict, (fields, pos)) -> Split { splitStrict = strict,
                                                splitFields = fields,
                                                splitPos = pos }, revfunc)
           (xpElem (gxFromString "Split")
                   (xpAttr (gxFromString "strict") xpPrim)
                   (xpPair (xpElemNodes (gxFromString "fields") mapPickler)
                           (xpElemNodes (gxFromString "pos") xpickle)))

typedPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] expty) =>
              PU [NodeG [] tag text] (Pattern expty)
typedPickler =
  let
    revfunc Typed { typedPat = pat, typedType = ty, typedPos = pos } =
      (pat, ty, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(pat, ty, pos) -> Typed { typedPat = pat, typedType = ty,
                                       typedPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Typed")
                        (xpTriple (xpElemNodes (gxFromString "pattern") xpickle)
                                  (xpElemNodes (gxFromString "type") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

asPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text,
              XmlPickler [NodeG [] tag text] expty) =>
             PU [NodeG [] tag text] (Pattern expty)
asPickler =
  let
    revfunc As { asName = sym, asPat = pat, asPos = pos } = (sym, (pat, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, (pat, pos)) -> As { asName = sym, asPat = pat,
                                       asPos = pos }, revfunc)
           (xpElem (gxFromString "As") xpickle
                   (xpPair (xpElemNodes (gxFromString "pattern") xpickle)
                           (xpElemNodes (gxFromString "pair") xpickle)))

namePickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [NodeG [] tag text] expty) =>
             PU [NodeG [] tag text] (Pattern expty)
namePickler =
  let
    revfunc Name { nameSym = sym, namePos = pos } = (sym, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, pos) -> Name { nameSym = sym, namePos = pos }, revfunc)
           (xpElem (gxFromString "Name") xpickle
                   (xpElemNodes (gxFromString "pos") xpickle))

exactPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] expty) =>
               PU [NodeG [] tag text] (Pattern expty)
exactPickler =
  let
    revfunc Exact { exactLit = v } = v
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Exact, revfunc) (xpElemNodes (gxFromString "Exact") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Pattern expty) where
  xpickle =
    let
      picker Option {} = 0
      picker Deconstruct {} = 1
      picker Split {} = 2
      picker Typed {} = 3
      picker As {} = 4
      picker Name {} = 5
      picker Exact {} = 6
    in
      xpAlt picker [optionPickler, deconstructPickler, splitPickler,
                    typedPickler, asPickler, namePickler, exactPickler]

compoundPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text,
                    XmlPickler [NodeG [] tag text] resty) =>
                   PU [NodeG [] tag text] (Exp resty)
compoundPickler =
  let
    revfunc Compound { compoundBody = body, compoundScope = scope,
                       compoundPos = pos } = (scope, (body, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(scope, (body, pos)) -> Compound { compoundBody = body,
                                                compoundScope = scope,
                                                compoundPos = pos }, revfunc)
           (xpElem (gxFromString "Compound") xpickle
                   (xpPair (xpElemNodes (gxFromString "body")
                                        (xpList xpickle))
                           (xpElemNodes (gxFromString "pos") xpickle)))

absPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler [NodeG [] tag text] resty) =>
              PU [NodeG [] tag text] (Exp resty)
absPickler =
  let
    revfunc Abs { absKind = kind, absCases = cases, absPos = pos } =
      (kind, (cases, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(kind, (cases, pos)) -> Abs { absKind = kind, absCases = cases,
                                           absPos = pos }, revfunc)
           (xpElem (gxFromString "Abs") xpickle
                   (xpPair (xpElemNodes (gxFromString "fields") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

matchPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] resty) =>
                PU [NodeG [] tag text] (Exp resty)
matchPickler =
  let
    revfunc Match { matchVal = val, matchCases = cases, matchPos = pos } =
      (val, cases, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(val, cases, pos) -> Match { matchVal = val, matchCases = cases,
                                          matchPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Match")
                        (xpTriple (xpElemNodes (gxFromString "value") xpickle)
                                  (xpElemNodes (gxFromString "cases") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

ascribePickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text,
                   XmlPickler [NodeG [] tag text] resty) =>
                  PU [NodeG [] tag text] (Exp resty)
ascribePickler =
  let
    revfunc Ascribe { ascribeVal = val, ascribeType = ty, ascribePos = pos } =
      (val, ty, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(val, ty, pos) -> Ascribe { ascribeVal = val, ascribeType = ty,
                                         ascribePos = pos }, revfunc)
           (xpElemNodes (gxFromString "Ascribe")
                        (xpTriple (xpElemNodes (gxFromString "value") xpickle)
                                  (xpElemNodes (gxFromString "type") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

seqPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler [NodeG [] tag text] resty) =>
              PU [NodeG [] tag text] (Exp resty)
seqPickler =
  let
    revfunc Seq { seqExps = exps, seqPos = pos } = (exps, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(exps, pos) -> Seq { seqExps = exps, seqPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Seq")
                        (xpPair (xpElemNodes (gxFromString "exps") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

callPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [NodeG [] tag text] resty) =>
              PU [NodeG [] tag text] (Exp resty)
callPickler =
  let
    revfunc Call { callFunc = func, callArgs = args, callPos = pos } =
      (func, args, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(func, args, pos) ->
             Call { callFunc = func, callArgs = args, callPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Call")
                        (xpTriple (xpElemNodes (gxFromString "func") xpickle)
                                  (xpElemNodes (gxFromString "args") mapPickler)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

recordPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] resty) =>
                PU [NodeG [] tag text] (Exp resty)
recordPickler =
  let
    revfunc Record { recordFields = fields, recordPos = pos } = (fields, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(fields, pos) -> Record { recordFields = fields,
                                       recordPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Record")
                        (xpPair (xpElemNodes (gxFromString "fields") mapPickler)
                                (xpElemNodes (gxFromString "pos") xpickle)))

recordTypePickler :: (GenericXMLString tag, Show tag,
                      GenericXMLString text, Show text,
                      XmlPickler [NodeG [] tag text] resty) =>
                     PU [NodeG [] tag text] (Exp resty)
recordTypePickler =
  let
    revfunc RecordType { recordTypeFields = fields,
                         recordTypePos = pos } = (fields, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(fields, pos) -> RecordType { recordTypeFields = fields,
                                           recordTypePos = pos }, revfunc)
           (xpElemNodes (gxFromString "RecordType")
                        (xpPair (xpElemNodes (gxFromString "fields") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

tuplePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] resty) =>
                PU [NodeG [] tag text] (Exp resty)
tuplePickler =
  let
    revfunc Tuple { tupleFields = fields, tuplePos = pos } = (fields, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(fields, pos) -> Tuple { tupleFields = fields,
                                      tuplePos = pos }, revfunc)
           (xpElemNodes (gxFromString "Tuple")
                        (xpPair (xpElemNodes (gxFromString "fields") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

projectPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text,
                   XmlPickler [NodeG [] tag text] resty) =>
                  PU [NodeG [] tag text] (Exp resty)
projectPickler =
  let
    revfunc Project { projectFields = fields, projectVal = val,
                      projectPos = pos } = (fields, val, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(fields, val, pos) -> Project { projectFields = fields,
                                             projectVal = val,
                                             projectPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Project")
                        (xpTriple (xpElemNodes (gxFromString "value") xpickle)
                                  (xpElemNodes (gxFromString "fields") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

symPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text,
              XmlPickler [NodeG [] tag text] resty) =>
             PU [NodeG [] tag text] (Exp resty)
symPickler =
  let
    revfunc Sym { symRef = sym, symPos = pos } = (sym, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(sym, pos) -> Sym { symRef = sym, symPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Sym")
                        (xpPair (xpElemNodes (gxFromString "ref") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

withPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [NodeG [] tag text] resty) =>
               PU [NodeG [] tag text] (Exp resty)
withPickler =
  let
    revfunc With { withVal = val, withArgs = args, withPos = pos } =
      (val, args, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(val, args, pos) -> With { withVal = val, withArgs = args,
                                        withPos = pos }, revfunc)
           (xpElemNodes (gxFromString "With")
                        (xpTriple (xpElemNodes (gxFromString "value") xpickle)
                                  (xpElemNodes (gxFromString "args") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

wherePickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] resty) =>
                PU [NodeG [] tag text] (Exp resty)
wherePickler =
  let
    revfunc Where { whereVal = val, whereProp = prop, wherePos = pos } =
      (val, prop, pos)
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(val, prop, pos) -> Where { whereVal = val, whereProp = prop,
                                         wherePos = pos }, revfunc)
           (xpElemNodes (gxFromString "Where")
                        (xpTriple (xpElemNodes (gxFromString "value") xpickle)
                                  (xpElemNodes (gxFromString "prop") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

anonPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                XmlPickler [NodeG [] tag text] resty) =>
               PU [NodeG [] tag text] (Exp resty)
anonPickler =
  let
    revfunc Anon { anonKind = kind, anonParams = params, anonContent = body,
                   anonSuperTypes = supers, anonPos = pos } =
      (kind, (params, supers, body, pos))
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (\(kind, (params, supers, body, pos)) ->
             Anon { anonKind = kind, anonParams = params, anonContent = body,
                    anonSuperTypes = supers, anonPos = pos }, revfunc)
           (xpElem (gxFromString "Anon") xpickle
                   (xp4Tuple (xpElemNodes (gxFromString "params") xpickle)
                             (xpElemNodes (gxFromString "supers") xpickle)
                             (xpElemNodes (gxFromString "body") xpickle)
                             (xpElemNodes (gxFromString "pos") xpickle)))

literalPickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text,
                   XmlPickler [NodeG [] tag text] resty) =>
                  PU [NodeG [] tag text] (Exp resty)
literalPickler =
  let
    revfunc Literal { literalVal = v } = v
    revfunc _ = error $! "Can't convert"
  in
    xpWrap (Literal, revfunc) (xpElemNodes (gxFromString "Literal") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] resty) =>
         XmlPickler [NodeG [] tag text] (Exp resty) where
  xpickle =
    let
      picker Compound {} = 0
      picker Abs {} = 1
      picker Match {} = 2
      picker Ascribe {} = 3
      picker Seq {} = 4
      picker Call {} = 5
      picker Record {} = 6
      picker RecordType {} = 7
      picker Tuple {} = 8
      picker Project {} = 9
      picker Sym {} = 10
      picker With {} = 11
      picker Where {} = 12
      picker Anon {} = 13
      picker Literal {} = 14
    in
      xpAlt picker [compoundPickler, absPickler, matchPickler, ascribePickler,
                    seqPickler, callPickler, recordPickler, recordTypePickler,
                    tuplePickler, projectPickler, symPickler, withPickler,
                    wherePickler, anonPickler, literalPickler]

makeArray :: [a] -> Array Word a
makeArray l = listArray (1, fromIntegral (length l)) l

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Entry expty) where
  xpickle =
    xpWrap (\(pat, pos) -> Entry { entryPat = pat, entryPos = pos },
            \Entry { entryPat = pat, entryPos = pos } -> (pat, pos))
           (xpElemNodes (gxFromString "Entry")
                        (xpPair (xpElemNodes (gxFromString "val") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Fields expty) where
  xpickle = xpWrap (\(bindings, order) ->
                     Fields { fieldsBindings = bindings,
                              fieldsOrder = makeArray order },
                    \Fields { fieldsBindings = bindings,
                              fieldsOrder = order } -> (bindings, elems order))
                   (xpElemNodes (gxFromString "Fields")
                                (xpPair mapPickler (xpList xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Field expty) where
  xpickle =
    xpWrap (\(val, pos) -> Field { fieldVal = val, fieldPos = pos },
            \Field { fieldVal = val, fieldPos = pos } -> (val, pos))
           (xpElemNodes (gxFromString "Field")
                        (xpPair (xpElemNodes (gxFromString "val") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Case expty) where
  xpickle =
    xpWrap (\(pat, body, pos) -> Case { casePat = pat, caseBody = body,
                                        casePos = pos },
            \Case { casePat = pat, caseBody = body, casePos = pos } ->
            (pat, body, pos))
           (xpElemNodes (gxFromString "Case")
                        (xpTriple (xpElemNodes (gxFromString "pattern") xpickle)
                                  (xpElemNodes (gxFromString "body") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))
