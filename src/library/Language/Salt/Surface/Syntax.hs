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

-- | Abstract Syntax structure.  This represents the surface language
-- in a more abstract form than is found in the AST structure.
module Language.Salt.Surface.Syntax(
       Assoc(..),
       Fixity(..),
       Syntax(..),
       Truth(..),
       Proof(..),
       Scope(..),
       Builder(..),
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

import Control.Monad.Positions
import Control.Monad.Symbols
import Data.Array
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.Position
import Data.Symbol
import Data.Word
import Language.Salt.Surface.Common
import Prelude hiding (init, exp)
import Text.FormatM

import qualified Data.HashMap.Strict as HashMap

data Assoc = Left | Right | NonAssoc
  deriving (Ord, Eq, Enum, Show)

data Fixity = Prefix | Infix !Assoc | Postfix
  deriving (Ord, Eq, Show)

-- | Syntax information for a symbol.
data Syntax =
  Syntax {
    -- | Fixity (and associativity).
    syntaxFixity :: !Fixity,
    -- | Precedence relations.
    syntaxPrecs :: ![(Ordering, Exp)]
  }
  deriving Eq

-- | Truths.  These are similar to declarations, except that they
-- affect the proof environment.  These include theorems and
-- invariants.
data Truth =
  Truth {
    -- | The class of the truth.
    truthKind :: !TruthKind,
    -- | The visibility of the truth.
    truthVisibility :: !Visibility,
    -- | The truth proposition.
    truthContent :: !Exp,
    -- | The position in source from which this arises.
    truthPos :: !Position
  }

-- | A static scope.  Elements are split up by kind, into builder definitions,
-- syntax directives, truths, proofs, and regular definitions.
data Scope =
  Scope {
    scopeBuilders :: !(HashMap Symbol Builder),
    -- | The syntax directives for this scope.
    scopeSyntax :: !(HashMap Symbol Syntax),
    -- | The truth environment for this scope.  This contains all
    -- theorems, axioms, and invariants.
    scopeTruths :: !(HashMap Symbol Truth),
    -- | All concrete definitions for this scope.
    scopeElems :: !(Array Visibility [Element]),
    -- | Proofs given in this scope.
    scopeProofs :: ![Proof]
  }
  deriving Eq

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

-- | Directives.  These are static commands, like imports or proofs,
-- which influence a scope, but do not directly define anything.
data Proof =
    -- | A proof.  This is just a code block with the name of the
    -- theorem being proven.
    Proof {
      -- | The name of the theorem being proven.
      proofName :: !Exp,
      -- | The body of the proof.
      proofBody :: !Exp,
      -- | The position in source from which this arises.
      proofPos :: !Position
    }

-- | Scope elements.  These represent declarations and imports
-- statements inside a scope.
data Element =
    -- | Value definitions.  These are declarations coupled with
    -- values.  These include function declarations.
    Def {
      -- | The binding pattern.
      defPattern :: !Pattern,
      -- | The value's initializer.
      defInit :: !(Maybe Exp),
      -- | The position in source from which this arises.
      defPos :: !Position
    }
    -- | An import directive.
  | Import {
      -- | The name(s) to import.
      importExp :: !Exp,
      -- | The position in source from which this arises.
      importPos :: !Position
    }

-- | Compound expression elements.  These are either "ordinary"
-- expressions, or declarations.
data Compound =
    -- | An ordinary expression.
    Exp !Exp
    -- | A regular definition.
  | Element !Element
    -- | A dynamic truth definition.
  | Dynamic {
      -- | Name of the truth definition.
      dynamicName :: !Symbol,
      -- | The dynamic truth definition.
      dynamicTruth :: !Truth
    }
    -- | A local builder definition.
  | Local {
      -- | The name of the builder.
      localName :: !Symbol,
      -- | The local builder definition.
      localBuilder :: !Builder
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
      -- | Syntax directives that occurred in the compound expression.
      compoundSyntax :: !(HashMap Symbol Syntax),
      -- | Proofs given in the compound expression.
      compoundProofs :: ![Proof],
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

instance Eq Truth where
  Truth { truthKind = kind1, truthVisibility = vis1,
          truthContent = content1 } ==
    Truth { truthKind = kind2, truthVisibility = vis2,
            truthContent = content2 } =
      kind1 == kind2 && vis1 == vis2 && content1 == content2

instance Eq Builder where
  Builder { builderKind = kind1, builderVisibility = vis1,
            builderParams = params1, builderSuperTypes = supers1,
            builderContent = content1 } ==
    Builder { builderKind = kind2, builderVisibility = vis2,
              builderParams = params2, builderSuperTypes = supers2,
              builderContent = content2 } =
      kind1 == kind2 && vis1 == vis2 && params1 == params2 &&
      supers1 == supers2 && content1 == content2

instance Eq Proof where
  Proof { proofName = name1, proofBody = body1 } ==
    Proof { proofName = name2, proofBody = body2 } =
      name1 == name2 && body1 == body2

instance Eq Element where
  Def { defPattern = pat1, defInit = init1 } ==
    Def { defPattern = pat2, defInit = init2 } =
      pat1 == pat2 && init1 == init2
  Import { importExp = exp1 } == Import { importExp = exp2 } = exp1 == exp2
  _ == _ = False

instance Eq Compound where
  Exp exp1 == Exp exp2 = exp1 == exp2
  Element elem1 == Element elem2 = elem1 == elem2
  Dynamic { dynamicName = name1, dynamicTruth = truth1 } ==
    Dynamic { dynamicName = name2, dynamicTruth = truth2 } =
      name1 == name2 && truth1 == truth2
  Local { localName = name1, localBuilder = builder1 } ==
    Local { localName = name2, localBuilder = builder2 } =
      name1 == name2 && builder1 == builder2
  _ == _ = False

instance Eq Pattern where
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
  Exact lit1 == Exact lit2 = lit1 == lit2
  _ == _ = False

instance Eq Exp where
  Compound { compoundSyntax = syntax1, compoundProofs = proofs1,
             compoundBody = body1 } ==
    Compound { compoundSyntax = syntax2, compoundProofs = proofs2,
               compoundBody = body2 } =
      syntax1 == syntax2 && proofs1 == proofs2 && body1 == body2
  Abs { absKind = kind1, absCases = cases1 } ==
    Abs { absKind = kind2, absCases = cases2 } =
      kind1 == kind2 && cases1 == cases2
  Match { matchVal = val1, matchCases = cases1 } ==
    Match { matchVal = val2, matchCases = cases2 } =
      val1 == val2 && cases1 == cases2
  Ascribe { ascribeVal = val1, ascribeType = ty1 } ==
    Ascribe { ascribeVal = val2, ascribeType = ty2 } =
      val1 == val2 && ty1 == ty2
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
  Sym { symName = sym1 } == Sym { symName = sym2 } = sym1 == sym2
    -- | A with expression.  Represents currying.
  With { withVal = val1, withArgs = args1 } ==
    With { withVal = val2, withArgs = args2 } =
      val1 == val2 && args1 == args2
    -- | Where expression.  Constructs refinement types.
  Where { whereVal = val1, whereProp = prop1 } ==
    Where { whereVal = val2, whereProp = prop2 } =
      val1 == val2 && prop1 == prop2
    -- | Builder literal.
  Anon { anonKind = kind1, anonSuperTypes = supers1,
         anonParams = fields1, anonContent = content1 } ==
    Anon { anonKind = kind2, anonSuperTypes = supers2,
           anonParams = fields2, anonContent = content2 } =
      kind1 == kind2 && supers1 == supers2 &&
      fields1 == fields2 && content1 == content2
  Literal lit1 == Literal lit2 = lit1 == lit2
  _ == _ = False

instance Eq Entry where
  Entry { entryPat = pat1 } == Entry { entryPat = pat2 } = pat1 == pat2

instance Eq Fields where
  Fields { fieldsBindings = bindings1, fieldsOrder = order1 } ==
    Fields { fieldsBindings = bindings2, fieldsOrder = order2 } =
      bindings1 == bindings2 && order1 == order2

instance Eq Field where
  Field { fieldVal = val1 } == Field { fieldVal = val2 } = val1 == val2

instance Eq Case where
  Case { casePat = pat1, caseBody = body1 } ==
    Case { casePat = pat2, caseBody = body2 } =
      pat1 == pat2 && body1 == body2

instance Hashable Assoc where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable Fixity where
  hashWithSalt s Prefix = hashWithSalt s (0 :: Int)
  hashWithSalt s (Infix assoc) =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` assoc
  hashWithSalt s Postfix = hashWithSalt s (2 :: Int)

instance Format Assoc where format = string . show
instance Format Fixity where format = string . show

instance (MonadSymbols m, MonadPositions m) => FormatM m (Ordering, Exp) where
  formatM (ord, exp) =
    let
      orddoc = case ord of
        LT -> string "<"
        EQ -> string "=="
        GT -> string ">"
    in do
      expdoc <- formatM exp
      return $! constructorDoc (string "Prec") [(string "ord", orddoc),
                                                (string "name", expdoc)]

instance (MonadSymbols m, MonadPositions m) => FormatM m Syntax where
  formatM Syntax { syntaxFixity = fixity, syntaxPrecs = precs } =
    do
      precdocs <- mapM formatM precs
      return $! constructorDoc (string "Syntax")
                               [(string "fixity", format fixity),
                                (string "scope", listDoc precdocs)]

instance (MonadSymbols m, MonadPositions m) => FormatM m Truth where
  formatM Truth { truthVisibility = vis, truthContent = content,
                  truthKind = kind, truthPos = pos } =
    do
      posdoc <- formatM pos
      contentdoc <- formatM content
      return (constructorDoc (string "Truth")
                             [(string "visibility", format vis),
                              (string "kind", format kind),
                              (string "pos", posdoc),
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

formatElems :: (MonadSymbols m, MonadPositions m) =>
               Array Visibility [Element] -> m Doc
formatElems arr =
  do
    hiddendocs <- mapM formatM (arr ! Hidden)
    privatedocs <- mapM formatM (arr ! Private)
    protecteddocs <- mapM formatM (arr ! Protected)
    publicdocs <- mapM formatM (arr ! Public)
    return $! constructorDoc (string "Elems")
                             [(string "hidden", listDoc hiddendocs),
                              (string "private", listDoc privatedocs),
                              (string "protected", listDoc protecteddocs),
                              (string "public", listDoc publicdocs)]

instance (MonadSymbols m, MonadPositions m) => FormatM m Scope where
  formatM Scope { scopeBuilders = builders, scopeSyntax = syntax,
                  scopeTruths = truths, scopeElems = defs,
                  scopeProofs = proofs } =
    do
      buildersdoc <- formatMap builders
      syntaxdoc <- formatMap syntax
      truthsdoc <- formatMap truths
      elemsdoc <- formatElems defs
      proofsdoc <- mapM formatM proofs
      return $! constructorDoc (string "Scope")
                               [(string "builders", buildersdoc),
                                (string "syntax", syntaxdoc),
                                (string "truths", truthsdoc),
                                (string "elems", elemsdoc),
                                (string "proofs", listDoc proofsdoc)]

instance (MonadSymbols m, MonadPositions m) => FormatM m Builder where
  formatM Builder { builderVisibility = vis, builderKind = cls,
                    builderSuperTypes = supers, builderParams = params,
                    builderContent = body, builderPos = pos } =
    do
      posdoc <- formatM pos
      superdocs <- mapM formatM supers
      paramdocs <- formatM params
      bodydoc <- formatM body
      return (constructorDoc (string "Builder")
                             [(string "visibility", format vis),
                              (string "pos", posdoc),
                              (string "kind", format cls),
                              (string "params", paramdocs),
                              (string "supers", listDoc superdocs),
                              (string "body", bodydoc)])

instance (MonadSymbols m, MonadPositions m) => FormatM m Proof where
  formatM Proof { proofName = exp, proofBody = body, proofPos = pos } =
    do
      namedoc <- formatM exp
      posdoc <- formatM pos
      bodydoc <- formatM body
      return (constructorDoc (string "Proof")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "body", bodydoc)])

instance (MonadSymbols m, MonadPositions m) => FormatM m Element where
  formatM Def { defPattern = pat, defInit = Just init, defPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      initdoc <- formatM init
      return (constructorDoc (string "Group")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "init", initdoc)])
  formatM Def { defPattern = pat, defInit = Nothing, defPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (constructorDoc (string "Group")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc)])
  formatM Import { importExp = exp, importPos = pos } =
    do
      expdoc <- formatM exp
      posdoc <- formatM pos
      return (constructorDoc (string "Import")
                             [(string "exp", expdoc),
                              (string "pos", posdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Compound where
  formatM (Exp e) = formatM e
  formatM (Element e) = formatM e
  formatM Dynamic { dynamicName = sym, dynamicTruth = truth } =
    do
      symdoc <- formatM sym
      truthdoc <- formatM truth
      return (constructorDoc (string "Dynamic")
                             [(string "sym", symdoc),
                              (string "truth", truthdoc)])
  formatM Local { localName = sym, localBuilder = builder } =
    do
      symdoc <- formatM sym
      builderdoc <- formatM builder
      return (constructorDoc (string "Local")
                             [(string "sym", symdoc),
                              (string "builder", builderdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Pattern where
  formatM Option { optionPats = pats, optionPos = pos } =
    do
      posdoc <- formatM pos
      patsdoc <- mapM formatM pats
      return (constructorDoc (string "Options")
                             [(string "pos", posdoc),
                              (string "patterns", listDoc patsdoc)])
  formatM Deconstruct { deconstructName = sym, deconstructPat = pat,
                        deconstructPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (constructorDoc (string "Deconstruct")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "pat", patdoc)])
  formatM Split { splitFields = fields, splitStrict = True, splitPos = pos } =
    do
      fieldsdoc <- formatMap fields
      posdoc <- formatM pos
      return (constructorDoc (string "Split")
                             [(string "pos", posdoc),
                              (string "strict", string "true"),
                              (string "fields", fieldsdoc)])
  formatM Split { splitFields = fields, splitStrict = False, splitPos = pos } =
    do
      fieldsdoc <- formatMap fields
      posdoc <- formatM pos
      return (constructorDoc (string "Split")
                             [(string "pos", posdoc),
                              (string "strict", string "false"),
                              (string "fields", fieldsdoc)])
  formatM Typed { typedPat = pat, typedType = ty, typedPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      tydoc <- formatM ty
      return (constructorDoc (string "Typed")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "type", tydoc)])
  formatM As { asName = sym, asPat = pat, asPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (constructorDoc (string "As")
                             [(string "name", namedoc),
                              (string "pos", posdoc),
                              (string "pat", patdoc)])
  formatM Name { nameSym = sym, namePos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      return (constructorDoc (string "Name")
                             [(string "name", namedoc),
                              (string "pos", posdoc)])
  formatM (Exact e) = formatM e

instance (MonadPositions m, MonadSymbols m) => FormatM m Exp where
  formatM Compound { compoundSyntax = syntax, compoundProofs = proofs,
                     compoundBody = body, compoundPos = pos } =
    do
      syntaxdoc <- formatMap syntax
      proofsdoc <- mapM formatM proofs
      posdoc <- formatM pos
      bodydoc <- mapM formatM body
      return (constructorDoc (string "Compound")
                             [(string "pos", posdoc),
                              (string "syntax", syntaxdoc),
                              (string "proofs", listDoc proofsdoc),
                              (string "body", listDoc bodydoc)])
  formatM Abs { absKind = kind, absCases = cases, absPos = pos } =
    do
      posdoc <- formatM pos
      casedocs <- mapM formatM cases
      return (constructorDoc (string "Abs")
                             [(string "pos", posdoc),
                              (string "kind", format kind),
                              (string "cases", listDoc casedocs)])
  formatM Match { matchVal = val, matchCases = cases, matchPos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      casedocs <- mapM formatM cases
      return (constructorDoc (string "Match")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "cases", listDoc casedocs)])
  formatM Ascribe { ascribeVal = val, ascribeType = ty, ascribePos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      tydoc <- formatM ty
      return (constructorDoc (string "Ascribe")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "type", tydoc)])
  formatM Seq { seqExps = exps, seqPos = pos } =
    do
      posdoc <- formatM pos
      expdocs <- mapM formatM exps
      return (constructorDoc (string "Seq")
                             [(string "pos", posdoc),
                              (string "exps", listDoc expdocs)])
  formatM Record { recordFields = fields, recordPos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- formatMap fields
      return (constructorDoc (string "Record")
                             [(string "pos", posdoc),
                              (string "fields", fielddocs)])
  formatM RecordType { recordTypeFields = fields,
                       recordTypePos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- formatM fields
      return (constructorDoc (string "RecordType")
                             [(string "pos", posdoc),
                              (string "fields", fielddocs)])
  formatM Tuple { tupleFields = fields, tuplePos = pos } =
    do
      posdoc <- formatM pos
      fielddocs <- mapM formatM fields
      return (constructorDoc (string "Tuple")
                             [(string "pos", posdoc),
                              (string "fields", listDoc fielddocs)])
  formatM Project { projectVal = val, projectFields = fields,
                    projectPos = pos } =
    do
      fielddocs <- mapM formatM fields
      posdoc <- formatM pos
      valdoc <- formatM val
      return (constructorDoc (string "Project")
                             [(string "fields", listDoc fielddocs),
                              (string "pos", posdoc),
                              (string "value", valdoc)])
  formatM Sym { symName = sym, symPos = pos } =
    do
      namedoc <- formatM sym
      posdoc <- formatM pos
      return (constructorDoc (string "Sym")
                             [(string "name", namedoc),
                              (string "pos", posdoc)])
  formatM With { withVal = val, withArgs = args, withPos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      argdocs <- formatM args
      return (constructorDoc (string "With")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "arg", argdocs)])
  formatM Where { whereVal = val, whereProp = prop, wherePos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      propdoc <- formatM prop
      return (constructorDoc (string "Where")
                             [(string "pos", posdoc),
                              (string "val", valdoc),
                              (string "prop", propdoc)])
  formatM Anon { anonKind = cls, anonParams = params, anonContent = body,
                 anonSuperTypes = supers, anonPos = pos } =

    do
      posdoc <- formatM pos
      superdocs <- mapM formatM supers
      paramdocs <- formatM params
      bodydoc <- formatM body
      return (constructorDoc (string "Anon")
                             [(string "pos", posdoc),
                              (string "kind", format cls),
                              (string "params", paramdocs),
                              (string "supers", listDoc superdocs),
                              (string "body", bodydoc)])
  formatM (Literal l) = formatM l

instance (MonadPositions m, MonadSymbols m) => FormatM m Entry where
  formatM Entry { entryPat = pat, entryPos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      return (constructorDoc (string "Entry")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Fields where
  formatM Fields { fieldsBindings = bindings, fieldsOrder = order } =
    do
      bindingsdoc <- formatMap bindings
      orderdoc <- mapM formatM (elems order)
      return (constructorDoc (string "Fields")
                             [(string "bindings", bindingsdoc),
                              (string "value", listDoc orderdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Field where
  formatM Field { fieldVal = val, fieldPos = pos } =
    do
      posdoc <- formatM pos
      valdoc <- formatM val
      return (constructorDoc (string "Field")
                             [(string "pos", posdoc),
                              (string "value", valdoc)])

instance (MonadPositions m, MonadSymbols m) => FormatM m Case where
  formatM Case { casePat = pat, caseBody = body, casePos = pos } =
    do
      posdoc <- formatM pos
      patdoc <- formatM pat
      bodydoc <- formatM body
      return (constructorDoc (string "Case")
                             [(string "pos", posdoc),
                              (string "pattern", patdoc),
                              (string "body", bodydoc)])
