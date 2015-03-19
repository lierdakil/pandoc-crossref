import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Map as M

type RefMap = M.Map String Int

--from data-accessors
type Accessor r a  =  a -> r -> (a, r)

setProp :: Accessor r a -> a -> r -> r
setProp f x = snd . f x

getProp :: Accessor r a -> r -> a
getProp f = fst . f undefined

modifyProp :: Accessor r a -> (a -> a) -> r -> r
modifyProp f g rOld =
   let (a,rNew) = f (g a) rOld
   in  rNew

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
                       , figPrefix   :: String
                       , eqnPrefix   :: String
                       , tblPrefix   :: String
                       , outFormat   :: Maybe Format
                       }

--state monad
type WS a = State References a

main :: IO ()
main = toJSONFilter go

go :: Maybe Format -> Pandoc -> Pandoc
go fmt p@(Pandoc meta _) = evalState doWalk defaultReferences
  where
  doWalk = walkM (replaceAttrImages opts) p >>= walkM (replaceRefs opts)
  opts = Options {
      useCleveref = isJust $ lookupMeta "cref" meta
    , figureTitle = getMetaString "Figure" "figureTitle"
    , tableTitle  = getMetaString "Table" "tableTitle"
    , figPrefix   = getMetaString "fig." "figPrefix"
    , eqnPrefix   = getMetaString "eq." "eqnPrefix"
    , tblPrefix   = getMetaString "tbl." "tblPrefix"
    , outFormat   = fmt
  }
  getMetaString def name = getString def $ lookupMeta name meta
  getString _ (Just (MetaString s)) = s
  getString def _ = def

replaceAttrImages :: Options -> Block -> WS Block
replaceAttrImages opts (Para c)
  | [Image alt img, Str s] <- c
  , Just label <- getRefLabel s "fig"
  = do
    idxStr <- replaceAttr label imgRefs'
    let alt' = case outFormat opts of
          Just (Format "latex") ->
            RawInline (Format "tex") ("\\label{"++label++"}") : alt
          _  ->
            [Str $ figureTitle opts,Space,Str idxStr, Str ".",Space]++alt
    return $ Para [Image alt' (fst img,"fig:")]
  | [Math DisplayMath eq, Str s] <- c
  , Just label <- getRefLabel s "eq"
  = case outFormat opts of
      Just (Format "latex") ->
        let eqn = "\\begin{equation}"++eq++"\\label{"++label++"}\\end{equation}"
        in return $ Para [RawInline (Format "tex") eqn]
      _ -> do
        idxStr <- replaceAttr label eqnRefs'
        let eq' = eq++"\\qquad("++idxStr++")"
        return $ Para [Math DisplayMath eq']
replaceAttrImages opts (Table title align widths header cells)
  | Str s <- last title
  , Just label <- getRefLabel s "tbl"
  = do
    idxStr <- replaceAttr label tblRefs'
    let title' =
          case outFormat opts of
              Just (Format "latex") ->
                [RawInline (Format "tex") ("\\label{"++label++"}")]
              _  ->
                [Str $ tableTitle opts,Space,Str idxStr, Str ".",Space]
          ++ init title
    return $ Table title' align widths header cells
replaceAttrImages _ x = return x

getRefLabel :: String -> String -> Maybe String
getRefLabel attr refPrefix
  | "}" `isSuffixOf` attr
  = liftM init $ stripPrefix ("{#"++refPrefix++":") attr
  | otherwise = Nothing

replaceAttr :: String -> Accessor References RefMap -> WS String
replaceAttr label prop
  = do
    index <- liftM (1+) $ gets (M.size . getProp prop)
    modify $ modifyProp prop (M.insert label index)
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

lookupUnsafe :: Ord k => k -> M.Map k v -> v
lookupUnsafe = (fromMaybe undefined .) . M.lookup

replaceRefs :: Options -> Inline -> WS Inline
replaceRefs opts (Cite cits _)
  | Just prefix <- allCitsPrefix cits
  , Just (Format "latex") <- outFormat opts
  = replaceRefsLatex prefix opts cits
  | Just prefix <- allCitsPrefix cits
  = replaceRefsOther prefix opts cits
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
    if useCleveref opts then
      "\\cref{"++listLabels prefix "" "" cits++"}"
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
  let str = refprefix' ++ makeIndices (sort indices)
      refprefix = lookupUnsafe prefix prefMap opts
      refprefix' | null refprefix = []
                 | otherwise   = refprefix ++ " "
  return $ Str str

getRefIndex :: String -> Citation -> WS (Maybe Int)
getRefIndex prefix Citation{citationId=cid}
  | Just label <- stripPrefix prefix cid = gets (M.lookup label . getProp prop)
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
