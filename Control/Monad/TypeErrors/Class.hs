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

-- | A class of monads for outputting compiler messages.
module Control.Monad.TypeErrors.Class(
       MonadTypeErrors(..)
       ) where

import Language.Salt.Core.Syntax

-- | Monad class representing the different kinds of compiler error
-- messages that can be generated.
class Monad m => MonadTypeErrors m where
  -- | Log a type mismatch error.
  typeMismatch :: Term bound free
               -- ^ The term from which this originates.
               -> Term bound free
               -- ^ The expected type.
               -> Term bound free
               -- ^ The actual type.
               -> m ()
  -- | Log an error when a function type was expected, but the term
  -- wasn't of the right form.
  expectedFunction :: Term bound free
                   -- ^ The term from which this originates.
                   -> Term bound free
                   -- ^ The term's actual type.
                   -> m ()
  -- | Log an error when a record type was expected, but the term
  -- wasn't of the right form.
  expectedRecord :: Term bound free
                 -- ^ The term from which this originates.
                 -> Term bound free
                 -- ^ The term's actual type.
                 -> m ()
  -- | Log an error when a computation type was expected, but the term
  -- wasn't of the right form.
  expectedComp :: Term bound free
               -- ^ The term from which this originates.
               -> Term bound free
               -- ^ The term's actual type.
               -> m ()
  -- | Log an error for an unbounded-rank type.
  infiniteType :: Term bound free
               -- ^ The illegal type.
               -> Term bound free
               -- ^ The term's actual type.
               -> m ()
