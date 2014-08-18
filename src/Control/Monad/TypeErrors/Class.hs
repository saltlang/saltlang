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
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A class of monads for outputting compiler messages.
module Control.Monad.TypeErrors.Class(
       MonadTypeErrors(..)
       ) where

import Data.Pos
import Language.Salt.Core.Syntax

-- | Monad class representing the different kinds of compiler error
-- messages that can be generated.
class Monad m => MonadTypeErrors sym m where
  -- | Log a type mismatch error, when a term's actual type is not a
  -- subtype of its expected type.
  typeMismatch :: Term sym sym
               -- ^ The term from which this originates.
               -> Term sym sym
               -- ^ The expected type.
               -> Term sym sym
               -- ^ The actual type.
               -> m (Term sym sym)
               -- ^ A type to return instead of the expected type.

  -- | Log an error when a term with a particular type was expected,
  -- but the term wasn't of the right form.
  formMismatch :: Term sym sym
               -- ^ The term whose form is incorrect.
               -> Term sym sym
               -- ^ The term's expected type.
               -> m (Term sym sym)
               -- ^ A type to return instead of the expected type.

  -- | Log an error when term with a function type is expected, but
  -- the actual type is something else
  expectedFunction :: Term sym sym
                   -- ^ The term from which this originates.
                   -> Term sym sym
                   -- ^ The term's actual type.
                   -> m (Term sym sym)
                   -- ^ A type to return instead of the expected type.

  -- | Log an error for an unbounded-rank type.
  infiniteType :: Term sym sym
               -- ^ The illegal type.
               -> Term sym sym
               -- ^ The term's actual type.
               -> m (Term sym sym)
               -- ^ A type to return instead of the expected type.

  -- | Log an undefined symbol error.
  undefSym :: sym
           -- ^ The symbol that is not defined.
           -> Pos
           -- ^ The position at which this occurred.
           -> m (Term sym sym)
           -- ^ A type representing the type of undefined symbols.
