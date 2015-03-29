import Text.Pandoc
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.Pandoc.Shared (normalizeInlines,stringify)
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as M

data RefRec = RefRec { refIndex :: (Int, Int)
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
                             , curChap :: Int
                             , stMeta  :: Meta
                             , stTmplV :: String -> Maybe MetaValue
                             }

-- accessors
imgRefs' :: Accessor References RefMap
imgRefs' new r@References{imgRefs=old} = (old, r{imgRefs=new})

eqnRefs' :: Accessor References RefMap
eqnRefs' new r@References{eqnRefs=old} = (old, r{eqnRefs=new})

tblRefs' :: Accessor References RefMap
tblRefs' new r@References{tblRefs=old} = (old, r{tblRefs=new})

defaultReferences :: References
defaultReferences = References M.empty M.empty M.empty 0 nullMeta (const Nothing)

data Options = Options { useCleveref :: Bool
                       , sepChapters :: Bool
                       , figPrefix   :: [Inline]
                       , eqnPrefix   :: [Inline]
                       , tblPrefix   :: [Inline]
                       , chapDelim   :: [Inline]
                       , rangeDelim  :: [Inline]
                       , lofTitle    :: [Block]
                       , lotTitle    :: [Block]
                       , outFormat   :: Maybe Format
                       , figureTemplate :: WS [Inline]
                       , tableTemplate  :: WS [Inline]
                       }

--state monad
type WS a = State References a

main :: IO ()
main = toJSONFilter go

go :: Maybe Format -> Pandoc -> Pandoc
go fmt (Pandoc meta bs) = Pandoc meta $ evalState doWalk st
  where
  st = defaultReferences{stMeta=meta}
  doWalk =
    walkM (replaceAttrImages opts) bs
    >>= bottomUpM (replaceRefs opts)
    >>= bottomUpM (listOf opts)
  opts = Options {
      useCleveref = getMetaBool "cref"
    , sepChapters = getMetaBool "chapters"
    , figPrefix   = getMetaInlines "figPrefix"
    , eqnPrefix   = getMetaInlines "eqnPrefix"
    , tblPrefix   = getMetaInlines "tblPrefix"
    , chapDelim   = getMetaInlines "chapDelim"
    , rangeDelim  = getMetaInlines "rangeDelim"
    , lofTitle    = getMetaBlock "lofTitle"
    , lotTitle    = getMetaBlock "lotTitle"
    , outFormat   = fmt
    , figureTemplate = replaceTemplate $ getMetaInlines "figureTemplate"
    , tableTemplate  = replaceTemplate $ getMetaInlines "tableTemplate"
  }
  getMetaBool name = fromMaybe False $ lookupDefault name meta >>= toBool
  getMetaInlines name = fromMaybe [] $ lookupDefault name meta >>= toInlines
  getMetaBlock name = fromMaybe [] $ lookupDefault name meta >>= toBlocks

replaceTemplate :: [Inline] -> WS [Inline]
replaceTemplate = bottomUpM replace
  where
  replace (x@(Math DisplayMath var):xs) = liftM (++xs) $ getTemplateMetaVar var [x]
  replace x = return x

getTemplateMetaVar :: String -> [Inline] -> WS [Inline]
getTemplateMetaVar var def' = do
  meta <- gets stMeta
  tmplv <- gets stTmplV
  return
    $ fromMaybe def'
    $ (tmplv var `mplus` lookupDefault var meta) >>= toInlines

getDefaultMeta :: String -> Maybe MetaValue
getDefaultMeta varname = case varname of
  "figureTitle"    -> Just $ MetaInlines [Str "Figure"]
  "tableTitle"     -> Just $ MetaInlines [Str "Table"]
  "titleDelim"     -> Just $ MetaInlines [Str ":"]
  "chapDelim"      -> Just $ MetaInlines [Str "."]
  "rangeDelim"     -> Just $ MetaInlines [Str "-"]
  "figPrefix"      -> Just $ MetaInlines [Str "fig."]
  "eqnPrefix"      -> Just $ MetaInlines [Str "eq."]
  "tblPrefix"      -> Just $ MetaInlines [Str "tbl."]
  "lofTitle"       -> Just $ MetaBlocks [Header 1 nullAttr [Str "List of Figures"]]
  "lotTitle"       -> Just $ MetaBlocks [Header 1 nullAttr [Str "List of Tables"]]
  "figureTemplate" -> Just $ MetaInlines [var "figureTitle",Space,var "i",var "titleDelim",Space,var "t"]
  "tableTemplate"  -> Just $ MetaInlines [var "tableTitle",Space,var "i",var "titleDelim",Space,var "t"]
  _                -> Nothing
  where var = Math DisplayMath

lookupDefault :: String -> Meta -> Maybe MetaValue
lookupDefault name meta = lookupMeta name meta `mplus` getDefaultMeta name

toInlines :: MetaValue -> Maybe [Inline]
toInlines (MetaString s) = return $ getInlines $
    either (error . show) id $ readMarkdown def s
  where getInlines (Pandoc _ bs) = concatMap getInline bs
        getInline (Plain ils) = ils
        getInline (Para ils) = ils
        getInline _ = []
toInlines (MetaInlines s) = return s
toInlines _ = Nothing

toBool :: MetaValue -> Maybe Bool
toBool (MetaBool b) = return b
toBool _ = Nothing

toBlocks :: MetaValue -> Maybe [Block]
toBlocks (MetaBlocks bs) = return bs
toBlocks (MetaInlines ils) = return [Plain ils]
toBlocks (MetaString s) = return $ getBlocks $
  either (error . show) id $ readMarkdown def s
  where getBlocks (Pandoc _ bs) = bs
toBlocks _ = Nothing

applyTemplate :: [Inline] -> [Inline] -> [Inline] -> WS [Inline]
applyTemplate i t tmpl = do
  setTmplV $ \n -> case n of
    "i" -> Just $ MetaInlines i
    "t" -> Just $ MetaInlines t
    _   -> Nothing
  res <- replaceTemplate tmpl
  setTmplV $ const Nothing
  return res
  where setTmplV f = modify $ \s -> s{stTmplV=f}

replaceAttrImages :: Options -> Block -> WS Block
replaceAttrImages opts x@(Header 1 _ _)
  | sepChapters opts
  = do
    modify (\r@References{curChap=cc} -> r{curChap=cc+1})
    return x
replaceAttrImages opts (Para (Image alt img:c))
  | Just label <- getRefLabel "fig" c
  = do
    idxStr <- replaceAttr opts label alt imgRefs'
    alt' <- case outFormat opts of
          Just f | isFormat "latex" f -> return $
            RawInline (Format "tex") ("\\label{"++label++"}") : alt
          _  -> figureTemplate opts >>= applyTemplate idxStr alt
    return $ Para [Image alt' (fst img,"fig:")]
replaceAttrImages opts (Para (Math DisplayMath eq:c))
  | Just label <- getRefLabel "eq" c
  = case outFormat opts of
      Just f | isFormat "latex" f ->
        let eqn = "\\begin{equation}"++eq++"\\label{"++label++"}\\end{equation}"
        in return $ Para [RawInline (Format "tex") eqn]
      _ -> do
        idxStr <- replaceAttr opts label [] eqnRefs'
        let eq' = eq++"\\qquad("++stringify idxStr++")"
        return $ Para [Math DisplayMath eq']
replaceAttrImages opts (Table title align widths header cells)
  | not $ null title
  , Just label <- getRefLabel "tbl" [last title]
  = do
    idxStr <- replaceAttr opts label (init title) tblRefs'
    title' <-
          case outFormat opts of
              Just f | isFormat "latex" f -> return $
                RawInline (Format "tex") ("\\label{"++label++"}") : init title
              _  -> tableTemplate opts >>= applyTemplate idxStr (init title)
    return $ Table title' align widths header cells
replaceAttrImages _ x = return x

getRefLabel :: String -> [Inline] -> Maybe String
getRefLabel _ [] = Nothing
getRefLabel tag ils
  | Str attr <- last ils
  , all (==Space) (init ils)
  , "}" `isSuffixOf` attr
  , ("{#"++tag++":") `isPrefixOf` attr
  = init `fmap` stripPrefix "{#" attr
getRefLabel _ _ = Nothing

replaceAttr :: Options -> String -> [Inline] -> Accessor References RefMap -> WS [Inline]
replaceAttr o label title prop
  = do
    chap  <- gets curChap
    index <- (1+) `fmap` gets (M.size . getProp prop)
    modify $ modifyProp prop $ M.insert label RefRec {
      refIndex=(chap,index)
    , refTitle=title
    }
    if sepChapters o
    then return $ Str (show chap) : chapDelim o ++ [Str (show index)]
    else return [Str (show index)]

-- accessors to state variables
accMap :: M.Map String (Accessor References RefMap)
accMap = M.fromList [("fig:",imgRefs')
                    ,("eq:" ,eqnRefs')
                    ,("tbl:",tblRefs')
                    ]

-- accessors to options
prefMap :: M.Map String (Options -> [Inline])
prefMap = M.fromList [("fig:",figPrefix)
                     ,("eq:" ,eqnPrefix)
                     ,("tbl:",tblPrefix)
                     ]

prefixes :: [String]
prefixes = M.keys accMap

getRefPrefix :: Options -> String -> [Inline]
getRefPrefix opts prefix | null refprefix = []
                         | otherwise   = refprefix ++ [Space]
                         where refprefix = lookupUnsafe prefix prefMap opts

lookupUnsafe :: Ord k => k -> M.Map k v -> v
lookupUnsafe = (fromMaybe undefined .) . M.lookup

replaceRefs :: Options -> [Inline] -> WS [Inline]
replaceRefs opts (Cite cits _:xs)
  | Just prefix <- allCitsPrefix cits
  = (++ xs) `fmap` replaceRefs' prefix opts cits
  where
    replaceRefs' = case outFormat opts of
                    Just f | isFormat "latex" f -> replaceRefsLatex
                    _                           -> replaceRefsOther
replaceRefs _ x = return x

allCitsPrefix :: [Citation] -> Maybe String
allCitsPrefix cits = foldl f Nothing prefixes
  where
  f x@(Just _) _ = x
  f _ p | all (isPrefixOf p . citationId) cits = Just p
  f _ _ = Nothing

replaceRefsLatex :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsLatex prefix opts cits =
  return $ p ++ [texcit]
  where
    texcit =
      RawInline (Format "tex") $
      if useCleveref opts then
        " \\cref{"++listLabels prefix "" "" cits++"}"
        else
          listLabels prefix " \\ref{" "}" cits
    p | useCleveref opts = []
      | otherwise = getRefPrefix opts prefix

listLabels :: String -> String -> String -> [Citation] -> String
listLabels prefix p s = foldl' joinStr "" . mapMaybe (getLabel prefix)
  where
  joinStr acc i | null acc  = p++i++s
                | otherwise = acc++","++p++i++s

getLabel :: String -> Citation -> Maybe String
getLabel prefix Citation{citationId=cid}
  | prefix `isPrefixOf` cid = Just cid
  | otherwise = Nothing

replaceRefsOther :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsOther prefix opts cits = do
  indices <- mapM (getRefIndex prefix) cits
  let
    indices' = groupBy ((==) `on` fmap fst) (sort indices)
  return $ getRefPrefix opts prefix ++ normalizeInlines (concatMap (makeIndices opts) indices')

getRefIndex :: String -> Citation -> WS (Maybe (Int, Int))
getRefIndex prefix Citation{citationId=cid}
  | prefix `isPrefixOf` cid
  = gets (fmap refIndex . M.lookup cid . getProp prop)
  | otherwise = return Nothing
  where
  prop = lookupUnsafe prefix accMap

makeIndices :: Options -> [Maybe (Int,Int)] -> [Inline]
makeIndices _ s | any isNothing s = [Str "??"]
makeIndices o s = intercalate sep $ reverse $ map f $ foldl' f2 [] $ catMaybes s
  where
  f2 [] i = [[i]]
  f2 ([]:xs) i = [i]:xs
  f2 l@(x@((_,hx):_):xs) i@(_,ni)
    | ni-hx == 0 = l        -- remove duplicates
    | ni-hx == 1 = (i:x):xs -- group sequental
    | otherwise     = [i]:l    -- new group
  f []  = []                          -- drop empty lists
  f [w] = show' w                    -- single value
  f [w1,w2] = show' w2 ++ sep ++ show' w1 -- two values
  f (x:xs) = show' (last xs) ++ rangeDelim o ++ show' x -- shorten more than two values
  sep = [Str ", "]
  show' (c,n) = if sepChapters o && c>0
    then [Str $ show c] ++ chapDelim o ++ [Str $ show n]
    else [Str $ show n]

listOf :: Options -> [Block] -> WS [Block]
listOf Options{outFormat=Just f} x | isFormat "latex" f = return x
listOf opts (Para [RawInline (Format "tex") "\\listoffigures"]:xs)
  = gets imgRefs >>= makeList (lofTitle opts) xs
listOf opts (Para [RawInline (Format "tex") "\\listoftables"]:xs)
  = gets tblRefs >>= makeList (lotTitle opts) xs
listOf _ x = return x

makeList :: [Block] -> [Block] -> M.Map String RefRec -> WS [Block]
makeList title xs refs
  = return $
      title ++
      OrderedList style (item `map` refsSorted)
      : xs
  where
    refsSorted = sortBy compare' $ M.toList refs
    compare' (_,RefRec{refIndex=i}) (_,RefRec{refIndex=j}) = compare i j
    item = (:[]) . Plain . refTitle . snd
    style = (1,DefaultStyle,DefaultDelim)

isFormat :: String -> Format -> Bool
isFormat fmt (Format f)
  | fmt == f = True
  | Just (x:_) <- stripPrefix fmt f
  , x `elem` "+-"
  = True
  | otherwise = False
