{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright (C) 2019  Nikolay Yakimov <root@livid.pp.ru>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Pandoc.CrossRef.References.Types.Monad (
    module Text.Pandoc.CrossRef.References.Types.Monad
  , module X
) where

import Text.Pandoc.CrossRef.References.Types.Ref
import Text.Pandoc.CrossRef.Util.Options.Types
import Text.Pandoc.CrossRef.Util.Settings.Types
import Control.Monad.State
import Control.Monad.Reader as X
import Control.Monad.Writer as X
import Control.Monad.Except as X
import qualified Control.Monad.Fail as Fail

data WSException = WSENoSuchPrefix String
                 | WSEDuplicateLabel String
                 | WSEFail String
                 deriving Show

type PureErr a = Either WSException a

-- | Enviromnent for 'CrossRefM'
data CrossRefEnv = CrossRefEnv {
                      creSettings :: Settings -- ^Metadata settings
                    , creOptions :: Options -- ^Internal pandoc-crossref options
                   }

-- | Essentially a reader monad for basic pandoc-crossref environment
type CrossRefM = ExceptT WSException (WriterT [String] (Reader CrossRefEnv))

newtype CrossRef a = CrossRef { unCrossRef :: CrossRefM a }
   deriving ( Functor, Applicative, Monad
           , MonadError WSException, MonadReader CrossRefEnv
           , MonadWriter [String] )
--state monad
newtype WS a = WS {
  unWS :: StateT References CrossRefM a
  } deriving ( Functor, Applicative, Monad, MonadState References
           , MonadError WSException, MonadReader CrossRefEnv
           , MonadWriter [String] )

instance Fail.MonadFail WS where
  fail s = throwError $ WSEFail s

pretty :: WSException -> String
pretty (WSENoSuchPrefix s) = "No such prefix: " <> s
pretty (WSEDuplicateLabel s) = "Duplicate label: " <> s
pretty (WSEFail s) = "Generic failure: " <> s
