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
