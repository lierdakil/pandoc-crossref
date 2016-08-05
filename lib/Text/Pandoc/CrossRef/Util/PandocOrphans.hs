{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Pandoc.CrossRef.Util.PandocOrphans where

import Text.Pandoc.Builder

instance {-# OVERLAPPING #-} ToMetaValue String where
  toMetaValue = MetaString
