{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright (C) 2015  Nikolay Yakimov <root@livid.pp.ru>

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

{-# LANGUAGE TemplateHaskell, RankNTypes, ViewPatterns, MultiWayIf #-}
module Text.Pandoc.CrossRef.Util.Settings.Template where

import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH hiding (Inline)
import Language.Haskell.TH.Syntax hiding (Inline)
import Text.Pandoc.Builder
import Text.Pandoc.CrossRef.Util.CustomLabels
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.Template

namedFields :: Con -> [VarStrictType]
namedFields (RecC _ fs) = fs
namedFields (ForallC _ _ c) = namedFields c
namedFields _ = []

fromRecDef :: forall t a r. Name -> t -> (Name -> Name -> Q [a]) -> (t -> [a] -> r) -> Q r
fromRecDef t cname f c = do
  info <- reify t
  reified <- case info of
                  TyConI dec -> return dec
                  _ -> fail "No cons"
  (_, cons) <- case reified of
               DataD _ _ params _ cons' _ -> return (params, cons')
               NewtypeD _ _ params _ con' _ -> return (params, [con'])
               _ -> fail "No cons"
  decs <- fmap concat . mapM (\ (name,_,_) -> f t name) . nub $ concatMap namedFields cons
  return $ c cname decs

nameDeriveSetters :: Name -> Q [Dec]
nameDeriveSetters t = fromRecDef t undefined (const makeAcc) (const id)

dropQualifiers :: Name -> Name
dropQualifiers (Name occ _) = mkName (occString occ)

makeAcc :: Name -> Q [Dec]
makeAcc (dropQualifiers -> accName) = do
    body <- [| Meta . M.singleton $(liftString $ show accName) . toMetaValue |]
    sig <- [t|forall a. ToMetaValue a => a -> Meta|]
    return
      [ SigD accName sig
      , ValD (VarP accName) (NormalB body) []
      ]

makeCon :: Name -> Name -> Q Exp
makeCon t cname = fromRecDef t cname makeCon' RecConE

makeCon' :: Name -> Name -> Q [(Name, Exp)]
makeCon' t accName = do
    VarI _ t' _ <- reify accName
    funT <- [t|$(conT t) -> Bool -> Int -> [Inline]|]
    inlT <- [t|$(conT t) -> [Inline]|]
    blkT <- [t|$(conT t) -> [Block]|]
    fmtT <- [t|$(conT t) -> Maybe Format|]
    boolT <- [t|$(conT t) -> Bool|]
    strT <- [t|$(conT t) -> Text|]
    intT <- [t|$(conT t) -> Int|]
    tmplT <- [t|$(conT t) -> Template|]
    btmplT <- [t|$(conT t) -> BlockTemplate|]
    idxTmplT <- [t|$(conT t) -> Text -> Template|]
    clT <- [t|$(conT t) -> Text -> Int -> Maybe Text|]
    chlT <- [t|$(conT t) -> Int -> Int -> Maybe Text|]
    let varName | Name (OccName n) _ <- accName = liftString n
    let dtv = return $ VarE $ mkName "dtv"
    body <-
      if
      | t' == boolT -> [|getMetaBool $(varName) $(dtv)|]
      | t' == intT -> [|read $ T.unpack $ getMetaString $(varName) $(dtv)|]
      | t' == funT -> [|tryCapitalizeM (flip (getMetaList (toInlines $(varName))) $(dtv)) $(varName)|]
      | t' == inlT -> [|getMetaInlines $(varName) $(dtv)|]
      | t' == blkT -> [|getMetaBlock $(varName) $(dtv)|]
      | t' == tmplT -> [|makeTemplate $(dtv) $ getMetaInlines $(varName) $(dtv)|]
      | t' == btmplT -> [|makeTemplate $(dtv) $ getMetaBlock $(varName) $(dtv)|]
      | t' == idxTmplT -> [|makeIndexedTemplate $(varName) $(dtv)|]
      | t' == clT -> [|customLabel $(dtv)|]
      | t' == chlT -> [|customHeadingLabel $(dtv)|]
      | t' == strT -> [|getMetaString $(varName) $(dtv)|]
      | t' == fmtT -> return $ VarE $ mkName "fmt"
      | otherwise -> fail $ show t'
    return [(accName, body)]
