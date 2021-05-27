{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, DeriveLift, StandaloneDeriving #-}
module Text.Pandoc.CrossRef.Util.Settings.LiftPandoc where

import Text.Pandoc.CrossRef.Util.Settings.Types
import Language.Haskell.TH.Syntax (Lift(..))
import Text.Pandoc
import Data.Map.Internal (Map(..))

deriving instance Lift Format
deriving instance Lift ColSpan
deriving instance Lift RowSpan
deriving instance Lift Alignment
deriving instance Lift Cell
deriving instance Lift Row
deriving instance Lift TableFoot
deriving instance Lift ListNumberDelim
deriving instance Lift ListNumberStyle
deriving instance Lift RowHeadColumns
deriving instance Lift TableBody
deriving instance Lift TableHead
deriving instance Lift ColWidth
deriving instance Lift Caption
deriving instance Lift Block
deriving instance Lift Citation
deriving instance Lift CitationMode
deriving instance Lift QuoteType
deriving instance Lift MathType
deriving instance Lift Inline
deriving instance Lift MetaValue
deriving instance (Lift k, Lift v) => Lift (Map k v)
deriving instance Lift Meta
deriving instance Lift Settings
