{-
This module is a part of data-accessor-template package
available at https://hackage.haskell.org/package/data-accessor-template-0.2.1.16

Distributed under the BSD-3-Clause license:

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

Neither the name of the <organization>; nor the names of its contributors
may be used to endorse or promote products derived from this software
without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE TemplateHaskell, LambdaCase, CPP #-}

{- |
This module provides an automatic Template Haskell
routine to scour data type definitions and generate
accessor objects for them automatically.
-}
module Data.Accessor.Template (
   nameDeriveAccessors, deriveAccessors,
   ) where

import qualified Data.Accessor.Basic as Accessor

import Language.Haskell.TH.Syntax
  -- (Q, Exp(VarE), Pat(VarP), Dec(ValD), Name(Name), mkOccName, occString, reify, )

import qualified Data.Traversable as Trav
import Data.List (nub)
import Data.List.HT (viewR)
import Data.Maybe (catMaybes)
import Control.Monad (when)



-- |@deriveAccessors n@ where @n@ is the name of a data type
-- declared with @data@ looks through all the declared fields
-- of the data type, and for each field ending in an underscore
-- generates an accessor of the same name without the underscore.
--
-- It is "nameDeriveAccessors" n f where @f@ satisfies
--
-- > f (s ++ "_") = Just s
-- > f x = Nothing -- otherwise
--
-- For example, given the data type:
--
-- > data Score = Score { p1Score_ :: Int
-- > , p2Score_ :: Int
-- > , rounds :: Int
-- > }
--
-- @deriveAccessors@ will generate the following objects:
--
-- > p1Score :: Accessor Score Int
-- > p1Score = Accessor p1Score_ (\x s -> s { p1Score_ = x })
-- > p2Score :: Accessor Score Int
-- > p2Score = Accessor p2Score_ (\x s -> s { p2Score_ = x })
--
-- It is used with Template Haskell syntax like:
--
-- > $( deriveAccessors ''TypeName )
--
-- And will generate accessors when TypeName was declared
-- using @data@ or @newtype@.
deriveAccessors :: Name -> Q [Dec]
deriveAccessors n = nameDeriveAccessors n stripUnderscore

stripUnderscore :: String -> Maybe String
stripUnderscore s = do
    (stem,'_') <- viewR s
    return stem

namedFields :: Con -> [VarStrictType]
namedFields (RecC _ fs) = fs
namedFields (ForallC _ _ c) = namedFields c
namedFields _ = []

-- |@nameDeriveAccessors n f@ where @n@ is the name of a data type
-- declared with @data@ and @f@ is a function from names of fields
-- in that data type to the name of the corresponding accessor. If
-- @f@ returns @Nothing@, then no accessor is generated for that
-- field.
nameDeriveAccessors :: Name -> (String -> Maybe String) -> Q [Dec]
nameDeriveAccessors t namer = do
    info <- reify t
    reified <- case info of
                    TyConI dec -> return dec
                    _ -> fail errmsg
    (params, cons) <- case reified of
                 DataD _ _ params _ cons' _ -> return (params, cons')
                 NewtypeD _ _ params _ con' _ -> return (params, [con'])
                 _ -> fail errmsg
    decs <- makeAccs params . nub $ concatMap namedFields cons
    when (null decs) $ qReport False nodefmsg
    return decs

    where

    errmsg = "Cannot derive accessors for name " ++ show t ++ " because"
          ++ "\n it is not a type declared with 'data' or 'newtype'"
          ++ "\n Did you remember to double-tick the type as in"
          ++ "\n $(deriveAccessors ''TheType)?"

    nodefmsg = "Warning: No accessors generated from the name " ++ show t
          ++ "\n If you are using deriveAccessors rather than"
          ++ "\n nameDeriveAccessors, remember accessors are"
          ++ "\n only generated for fields ending with an underscore"

    makeAccs :: [TyVarBndr] -> [VarStrictType] -> Q [Dec]
    makeAccs params vars =
        concat . catMaybes
        <$> mapM (\ (name,_,ftype) -> makeAccFromName name params ftype) vars

    transformName :: Name -> Maybe Name
    transformName (Name occ f) = do
        n <- namer (occString occ)
        return $ Name (mkOccName n) f

    makeAccFromName :: Name -> [TyVarBndr] -> Type -> Q (Maybe [Dec])
    makeAccFromName name params ftype =
        Trav.mapM (makeAcc name params ftype) $ transformName name

    -- haddock doesn't grok TH
#ifndef __HADDOCK__

    makeAcc ::Name -> [TyVarBndr] -> Type -> Name -> Q [Dec]
    makeAcc name params ftype accName = do
        let params' = map (\case (PlainTV n) -> n; (KindedTV n _) -> n) params
        let appliedT = foldl AppT (ConT t) (map VarT params')
        body <- [|
                 Accessor.fromSetGet
                    ( \x s ->
                        $( return $ RecUpdE (VarE 's) [(name, VarE 'x)] ) )
                    $( return $ VarE name )
                |]
        return
          [ SigD accName (ForallT (map PlainTV params')
               [] (AppT (AppT (ConT ''Accessor.T) appliedT) ftype))
          , ValD (VarP accName) (NormalB body) []
          ]

#endif
