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

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.Pandoc.CrossRef.Util.Meta
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Haskell.TH hiding (Inline)
import Language.Haskell.TH.Syntax hiding (Inline)
import Data.List
import Text.Pandoc.CrossRef.Util.Prefixes

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
    inlT <- [t|$(conT t) -> Inlines|]
    blkT <- [t|$(conT t) -> Blocks|]
    fmtT <- [t|$(conT t) -> Maybe Format|]
    boolT <- [t|$(conT t) -> Bool|]
    intT <- [t|$(conT t) -> Int|]
    pfxT <- [t|$(conT t) -> Prefixes|]
    strT <- [t|$(conT t) -> String|]
    mstT <- [t|$(conT t) -> Maybe T.Text|]
    let varName | Name (OccName n) _ <- accName = liftString n
        dtv = return $ VarE $ mkName "dtv"
        fmt = return $ VarE $ mkName "fmt"
    body <-
      if
      | t' == boolT -> [|getMetaBool $(varName) $(dtv)|]
      | t' == intT -> [|read . T.unpack $ getMetaString $(varName) $(dtv)|]
      | t' == inlT -> [|getMetaInlines $(varName) $(dtv)|]
      | t' == blkT -> [|getMetaBlock $(varName) $(dtv)|]
      | t' == fmtT -> fmt
      | t' == pfxT -> [|getPrefixes $(fmt) $(varName) $(dtv)|]
      | t' == strT -> [|getMetaString $(varName) $(dtv)|]
      | t' == mstT -> [|getMetaStringMaybe $(varName) $(dtv)|]
      | otherwise -> fail $ show t'
    return [(accName, body)]
