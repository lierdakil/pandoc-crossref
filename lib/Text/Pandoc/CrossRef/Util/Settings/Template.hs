{-# LANGUAGE TemplateHaskell, RankNTypes, ViewPatterns, UnicodeSyntax, MultiWayIf #-}
module Text.Pandoc.CrossRef.Util.Settings.Template where

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.Pandoc.CrossRef.Util.Meta
import qualified Data.Map as M
import Language.Haskell.TH hiding (Inline)
import Language.Haskell.TH.Syntax hiding (Inline)
import Data.List
import Control.Monad
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.CustomLabels (customLabel)

namedFields :: Con -> [VarStrictType]
namedFields (RecC _ fs) = fs
namedFields (ForallC _ _ c) = namedFields c
namedFields _ = []

fromRecDef :: ∀ t a r. Name -> t -> (Name -> Name -> Q [a]) -> (t -> [a] -> r) -> Q r
fromRecDef t cname f c = do
  info <- reify t
  reified <- case info of
                  TyConI dec -> return dec
                  _ -> fail "No cons"
  (_, cons) <- case reified of
               DataD _ _ params cons' _ -> return (params, cons')
               NewtypeD _ _ params con' _ -> return (params, [con'])
               _ -> fail "No cons"
  decs <- liftM concat . mapM (\ (name,_,_) -> f t name) . nub $ concatMap namedFields cons
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
    VarI _ t' _ _ <- reify accName
    funT <- [t|$(conT t) -> Bool -> Int -> [Inline]|]
    inlT <- [t|$(conT t) -> [Inline]|]
    blkT <- [t|$(conT t) -> [Block]|]
    fmtT <- [t|$(conT t) -> Maybe Format|]
    boolT <- [t|$(conT t) -> Bool|]
    intT <- [t|$(conT t) -> Int|]
    tmplT <- [t|$(conT t) -> Template|]
    clT <- [t|$(conT t) -> String -> Int -> Maybe String|]
    let varName | Name (OccName n) _ <- accName = liftString n
    let dtv = return $ VarE $ mkName "dtv"
    body <-
      if
      | t' == boolT -> [|getMetaBool $(varName) $(dtv)|]
      | t' == intT -> [|read $ getMetaString $(varName) $(dtv)|]
      | t' == funT -> [|tryCapitalizeM (flip (getMetaList toInlines) $(dtv)) $(varName)|]
      | t' == inlT -> [|getMetaInlines $(varName) $(dtv)|]
      | t' == blkT -> [|getMetaBlock $(varName) $(dtv)|]
      | t' == tmplT -> [|makeTemplate $(dtv) $ getMetaInlines $(varName) $(dtv)|]
      | t' == clT -> [|customLabel $(dtv)|]
      | t' == fmtT -> return $ VarE $ mkName "fmt"
      | otherwise -> fail $ show t'
    return [(accName, body)]
