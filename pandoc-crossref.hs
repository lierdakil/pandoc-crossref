import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.Pandoc.Generic
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Map as M

data RefRec = RefRec { refIndex :: Int
                     , refTitle :: [Inline]
                     }

type RefMap = M.Map String RefRec

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
-- end data-accessor

-- state data type
data References = References { imgRefs :: RefMap
                             , eqnRefs :: RefMap
                             , tblRefs :: RefMap
                             }

-- accessors
imgRefs' :: Accessor References RefMap
imgRefs' new r@References{imgRefs=old} = (old, r{imgRefs=new})

eqnRefs' :: Accessor References RefMap
eqnRefs' new r@References{eqnRefs=old} = (old, r{eqnRefs=new})

tblRefs' :: Accessor References RefMap
tblRefs' new r@References{tblRefs=old} = (old, r{tblRefs=new})

defaultReferences :: References
defaultReferences = References M.empty M.empty M.empty

data Options = Options { useCleveref :: Bool
                       , figureTitle :: String
                       , tableTitle  :: String
                       , titleDelim  :: String
                       , figPrefix   :: String
                       , eqnPrefix   :: String
                       , tblPrefix   :: String
                       , lofTitle   :: String
                       , lotTitle   :: String
                       , outFormat   :: Maybe Format
                       }

--state monad
type WS a = State References a

main :: IO ()
main = toJSONFilter go

go :: Maybe Format -> Pandoc -> Pandoc
go fmt p@(Pandoc meta _) = evalState doWalk defaultReferences
  where
  doWalk =
    walkM (replaceAttrImages opts) p
    >>= walkM (replaceRefs opts)
    >>= bottomUpM (listOf opts)
  opts = Options {
      useCleveref = isJust $ lookupMeta "cref" meta
    , figureTitle = getMetaString "Figure" "figureTitle"
    , tableTitle  = getMetaString "Table" "tableTitle"
    , titleDelim  = getMetaString ":" "titleDelimiter"
    , figPrefix   = getMetaString "fig." "figPrefix"
    , eqnPrefix   = getMetaString "eq." "eqnPrefix"
    , tblPrefix   = getMetaString "tbl." "tblPrefix"
    , lofTitle    = getMetaString "List of Figures" "lofTitle"
    , lotTitle    = getMetaString "List of Tables" "lotTitle"
    , outFormat   = fmt
  }
  getMetaString def name = getString def $ lookupMeta name meta
  getString _ (Just (MetaString s)) = s
  getString def _ = def

replaceAttrImages :: Options -> Block -> WS Block
replaceAttrImages opts (Para (Image alt img:c))
  | Just label <- getRefLabel "fig" c
  = do
    idxStr <- replaceAttr label alt imgRefs'
    let alt' = case outFormat opts of
          Just (Format "latex") ->
            RawInline (Format "tex") ("\\label{"++label++"}") : alt
          _  ->
            [Str $ figureTitle opts,Space,Str idxStr, Str $ titleDelim opts,Space]++alt
    return $ Para [Image alt' (fst img,"fig:")]
replaceAttrImages opts (Para (Math DisplayMath eq:c))
  | Just label <- getRefLabel "eq" c
  = case outFormat opts of
      Just (Format "latex") ->
        let eqn = "\\begin{equation}"++eq++"\\label{"++label++"}\\end{equation}"
        in return $ Para [RawInline (Format "tex") eqn]
      _ -> do
        idxStr <- replaceAttr label [] eqnRefs'
        let eq' = eq++"\\qquad("++idxStr++")"
        return $ Para [Math DisplayMath eq']
replaceAttrImages opts (Table title align widths header cells)
  | Just label <- getRefLabel "tbl" [last title]
  = do
    idxStr <- replaceAttr label (init title) tblRefs'
    let title' =
          case outFormat opts of
              Just (Format "latex") ->
                [RawInline (Format "tex") ("\\label{"++label++"}")]
              _  ->
                [Str $ tableTitle opts,Space,Str idxStr, Str $ titleDelim opts,Space]
          ++ init title
    return $ Table title' align widths header cells
replaceAttrImages _ x = return x

getRefLabel :: String -> [Inline] -> Maybe String
getRefLabel tag ils
  | Str attr <- last ils
  , all (==Space) (init ils)
  , "}" `isSuffixOf` attr
  = init `fmap` stripPrefix ("{#"++tag++":") attr
getRefLabel _ _ = Nothing

replaceAttr :: String -> [Inline] -> Accessor References RefMap -> WS String
replaceAttr label title prop
  = do
    index <- (1+) `fmap` gets (M.size . getProp prop)
    modify $ modifyProp prop $ M.insert label RefRec {
      refIndex=index
    , refTitle=title
    }
    return $ show index

-- accessors to state variables
accMap :: M.Map String (Accessor References RefMap)
accMap = M.fromList [("fig:",imgRefs')
                    ,("eq:" ,eqnRefs')
                    ,("tbl:",tblRefs')
                    ]

-- accessors to options
prefMap :: M.Map String (Options -> String)
prefMap = M.fromList [("fig:",figPrefix)
                     ,("eq:" ,eqnPrefix)
                     ,("tbl:",tblPrefix)
                     ]

prefixes :: [String]
prefixes = M.keys accMap

getRefPrefix :: Options -> String -> String
getRefPrefix opts prefix | null refprefix = []
                         | otherwise   = refprefix ++ " "
                         where refprefix = lookupUnsafe prefix prefMap opts

lookupUnsafe :: Ord k => k -> M.Map k v -> v
lookupUnsafe = (fromMaybe undefined .) . M.lookup

replaceRefs :: Options -> Inline -> WS Inline
replaceRefs opts (Cite cits _)
  | Just prefix <- allCitsPrefix cits
  = replaceRefs' prefix opts cits
  where
    replaceRefs' = case outFormat opts of
                    Just (Format "latex") -> replaceRefsLatex
                    _                     -> replaceRefsOther
replaceRefs _ x = return x

allCitsPrefix :: [Citation] -> Maybe String
allCitsPrefix cits = foldl f Nothing prefixes
  where
  f x@(Just _) _ = x
  f _ p | all (isPrefixOf p . citationId) cits = Just p
  f _ _ = Nothing

replaceRefsLatex :: String -> Options -> [Citation] -> WS Inline
replaceRefsLatex prefix opts cits =
  return $ RawInline (Format "tex") $
    getRefPrefix opts prefix ++
    if useCleveref opts then
      " \\cref{"++listLabels prefix "" "" cits++"}"
      else
        listLabels prefix " \\ref{" "}" cits

listLabels :: String -> String -> String -> [Citation] -> String
listLabels prefix p s = foldl' joinStr "" . mapMaybe (getLabel prefix)
  where
  joinStr acc i | null acc  = p++i++s
                | otherwise = acc++","++p++i++s

getLabel :: String -> Citation -> Maybe String
getLabel prefix Citation{citationId=cid}
  | Just label <- stripPrefix prefix cid = Just label
  | otherwise = Nothing

replaceRefsOther :: String -> Options -> [Citation] -> WS Inline
replaceRefsOther prefix opts cits = do
  indices <- mapM (getRefIndex prefix) cits
  return $ Str $ getRefPrefix opts prefix ++ makeIndices (sort indices)

getRefIndex :: String -> Citation -> WS (Maybe Int)
getRefIndex prefix Citation{citationId=cid}
  | Just label <- stripPrefix prefix cid
  = gets (fmap refIndex . M.lookup label . getProp prop)
  | otherwise = return Nothing
  where
  prop = lookupUnsafe prefix accMap

makeIndices :: [Maybe Int] -> String
makeIndices s | any isNothing s = "??"
makeIndices s = intercalate sep $ reverse $ mapMaybe f $ foldl' f2 [] $ catMaybes s
  where
  f2 [] i = [[i]]
  f2 l@(x:xs) i | i-head x == 0 = l        -- remove duplicates
                | i-head x == 1 = (i:x):xs -- group sequental
                | otherwise     = [i]:l    -- new group
  f []  = Nothing                          -- drop empty lists
  f [w] = Just $ show w                    -- single value
  f [w1,w2] = Just $ show w2 ++ sep ++ show w1 -- two values
  f (x:xs) = Just $ show (last xs) ++ "-" ++ show x -- shorten more than two values
  sep = ", "

listOf :: Options -> [Block] -> WS [Block]
listOf Options{outFormat=Just (Format "latex")} x = return x
listOf opts (Para [RawInline (Format "tex") "\\listoffigures"]:xs)
  = gets imgRefs >>= makeList (lofTitle opts) xs
listOf opts (Para [RawInline (Format "tex") "\\listoftables"]:xs)
  = gets tblRefs >>= makeList (lotTitle opts) xs
listOf _ x = return x

makeList :: String -> [Block] -> M.Map String RefRec -> WS [Block]
makeList title xs refs
  = return $
      Header 1 nullAttr [Str title]
      : OrderedList style (item `map` refsSorted)
      : xs
  where
    refsSorted = sortBy compare' $ M.toList refs
    compare' (_,RefRec{refIndex=i}) (_,RefRec{refIndex=j}) = compare i j
    item = (:[]) . Plain . refTitle . snd
    style = (1,DefaultStyle,DefaultDelim)
