module Text.Pandoc.CrossRef.Util.Settings.Types where

import Text.Pandoc.Definition
import qualified Data.Map as M

newtype Settings = Settings { unSettings :: Meta } deriving (Eq, Ord, Show)
newtype MetaSetting = MetaSetting MetaValue deriving (Eq, Ord, Show)

lookupSettings :: String -> Settings -> Maybe MetaValue
lookupSettings k (Settings s) = lookupMeta k s

instance Semigroup Settings where
  (Settings (Meta a)) <> (Settings (Meta b)) = Settings $ Meta $ M.unionWith merge a b

instance Monoid Settings where
  mappend = (<>)
  mempty = Settings nullMeta

merge :: MetaValue -> MetaValue -> MetaValue
merge (MetaMap m1) (MetaMap m2) = MetaMap $ M.unionWith merge m1 m2
merge x _ = x
