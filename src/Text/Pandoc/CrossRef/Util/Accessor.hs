module Text.Pandoc.CrossRef.Util.Accessor (Accessor, setProp, getProp, modifyProp) where

-- from data-accessor http://www.haskell.org/haskellwiki/Record_access
-- Copyright (c) Henning Thielemann <haskell@henning-thielemann.de>, Luke Palmer <lrpalmer@gmail.com>
-- Licensed under BSD3 -- see BSD3.md
type Accessor r a  =  a -> r -> (a, r)

setProp :: Accessor r a -> a -> r -> r
setProp f x = snd . f x

getProp :: Accessor r a -> r -> a
getProp f = fst . f undefined

modifyProp :: Accessor r a -> (a -> a) -> r -> r
modifyProp f g rOld =
   let (a,rNew) = f (g a) rOld
   in  rNew
