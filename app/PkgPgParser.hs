{-# LANGUAGE ViewPatterns,BangPatterns #-}
module PkgPgParser(PackageInfo(..),packageInfoExtract) where

import Prelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Read
--import qualified Data.Text.Lazy.IO as LTIO
import Control.Exception
import Text.Taggy.Lens
import Control.Arrow
import Data.Typeable
import Data.Time
import Data.Maybe
import Control.Lens hiding (elements,children)
import qualified Data.HashMap.Strict as HM
import Text.Taggy.Renderer
data HTMLParseException = HTMLParseException String  deriving Typeable

instance Exception HTMLParseException

instance Show HTMLParseException where
    show (HTMLParseException s) = show s


hackageStr2Time:: String -> UTCTime
hackageStr2Time = parseTimeOrError True defaultTimeLocale "%a %b %e %X %Z %Y"

data PackageInfo = PackageInfo  {   piName :: !T.Text
                                ,   piShortDesc :: !T.Text
                                ,   piDesc :: !T.Text
                                ,   piVer :: !T.Text
                                ,   piDepends :: ![T.Text]
                                ,   piAuthor :: !T.Text
                                ,   piCats :: ![T.Text]
                                ,   piHomePg :: !T.Text
                                ,   piReps :: !T.Text
                                ,   piLastUp :: !UTCTime
                                ,   piStackageVer :: !T.Text
                                ,   piStackageURL :: !T.Text
                                ,   piDownloads :: !Int
                                ,   piIsDoc :: !Bool
                                } deriving Show

delAnchors:: [Node] -> [Node]
delAnchors [] = []
delAnchors (n@(NodeElement Element {eltName = "a", eltAttrs = a}):xs)
            | maybe False ("#" `T.isPrefixOf`) $ "href" `HM.lookup` a = delAnchors xs
            | otherwise = n : delAnchors xs
delAnchors (NodeElement Element {eltName = n, eltAttrs = a, eltChildren=c}:xs) =
                NodeElement (Element n a $ delAnchors c) : delAnchors xs
delAnchors (x:xs) = x : delAnchors xs

packageInfoExtract' :: LT.Text -> PackageInfo
packageInfoExtract' src =
    let !parsed = chk "HTML parse error" $ src ^? htmlWith False . elements . named (only "html") 
        (piName,piShortDesc) = (t *** (t . LT.stripStart . LT.drop 1))  $ LT.break (':'==) $ LT.strip $ LT.fromStrict $ chk "<title> not found" $ (parsed ^.. allNamed (only "title")) ^? _head . children .  _head . content
        !cntnt = chk "id=content not found" $ (parsed ^.. allNamed (only "div") . attributed (ix "id" . only "content")) ^? _head . children . _tail . _tail
        extrDscr [] = throw $ HTMLParseException "id=properties not found"
        extrDscr (NodeElement c@Element {eltName = "div", eltAttrs = (("id" `HM.lookup`)-> Just "properties")}:_) = ([],c) 
        extrDscr (x:xs) = let (z,c) = extrDscr xs in (x:z,c)
        (!desc,!prpt)=extrDscr cntnt
        piDesc = t $ LT.concat $ map (renderWith False) $ delAnchors desc
        !ptbl = chk "Properties table not found" $ (prpt ^.. allNamed (only "table") . attributed (ix "class" . only "properties")) ^? _head . allNamed (only "tbody")
           . children 
        parseTbl [] = []
        parseTbl (NodeElement Element {eltName = "tr", eltChildren =
            [NodeElement Element {eltName = "th", eltChildren = [NodeContent k]},
             NodeElement v@Element {eltName = "td"} 
            ]}:xs) = (k,v):parseTbl xs
        parseTbl (_:xs) = parseTbl xs
        !tbl = HM.fromList $ parseTbl ptbl 
        getProp n = case HM.lookup n tbl of
                        Just v -> v
                        _ -> throw $ HTMLParseException $ concat ["Property ",T.unpack n," not found"]
        piVer = T.concat $ getProp "Versions" ^.. allNamed (only "strong") . contents
        getFromATags' v = v ^.. allNamed (only "a") . contents
        getFromATags n = getFromATags' (getProp n)
        getFromATagsOptional n = maybe [] getFromATags' $ n `HM.lookup` tbl 
        piDepends = filter (/=piName) $ getFromATags "Dependencies"
        getSimpleText n = getProp n ^. contents
        piAuthor = getSimpleText "Author"
        piCats = getFromATagsOptional "Category"
        head_t [] = T.empty
        head_t (x:_) = x
        piHomePg = T.strip $ head_t $ getFromATagsOptional "Home page"
        piReps = T.unwords $ getFromATagsOptional "Source repository"
        piLastUp = minimum $ map hackageStr2Time $ filter (not . null) $
            map (T.unpack . fst . T.breakOn " by " . view contents) $
            mapMaybe (`HM.lookup` tbl) ["Uploaded","Updated"]
        !stkg = getProp "Distributions" ^.. allNamed (only "a") . attributed (ix "href" . nearly "" (T.isInfixOf ".stackage.org/"))
        (piStackageURL,piStackageVer) = case stkg of
            [] -> (T.empty,T.empty) 
            (x:_) -> (x ^. attrs . at "href" . _Just, x ^. contents)
        !downloads' = T.tail $ T.dropWhile (/='(') $ getSimpleText "Downloads"
        piDownloads = either (const (0 :: Int)) fst (decimal downloads')
        piIsDoc = notElem "not" $ T.words $ getSimpleText "Status"
    in PackageInfo{..}
  where chk msg Nothing  = throw $ HTMLParseException msg 
        chk _ (Just v) = v
        t = LT.toStrict
        
packageInfoExtract :: LT.Text -> IO (Either String PackageInfo)
packageInfoExtract src = (Right <$> evaluate (packageInfoExtract' src)) `catches`
    [Handler (\e -> return $ Left $ show (e :: HTMLParseException)),
     Handler (\e -> return $ Left $ show (e :: SomeException))]
