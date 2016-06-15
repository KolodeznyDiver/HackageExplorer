{-# LANGUAGE ViewPatterns #-} 
module Handler.Home(getScanR,getHomeR,postHomeR,chkAppState) where

import Import
import Database.Persist.Sql
import Control.Monad.Extra
import Control.Concurrent
import qualified Data.Text as T
import ScanThread
import HackageExplorerDefs
import HackageExplorerAux
import Yesod.Form.Jquery
import AppWidgets

getScanR' :: Handler Html
getScanR' = defaultLayout $ do
                setTitle appTitle
                $(widgetFile "scan")

getScanR :: Handler Html
getScanR = do
    a@App{..} <- getYesod
    io $ do
        aSt <- atomically $ do
            s <- readTVar appState
            when (s==AppIdle) $ writeTVar appState AppScan
            return s
        when (aSt==AppIdle) $ void $ forkIO (scanThread a) 
    getScanR'
    
chkAppState:: Handler Html -> Handler Html
chkAppState h = do
    app <- getYesod
    aSt <- atomically $ readTVar $ appState app
    case aSt of
        AppScan -> getScanR'
        AppIdle -> h

data PkgSelectOpts = PkgSelectOpts
    { pkgSlctFrom     :: Day
    , pkgSlctDwnlds   :: Int
    , pkgSlctStackage :: Bool
    , pkgSlctCategs   :: Maybe Text
    }
  deriving Show

pkgSelectForm :: Html -> MForm Handler (FormResult PkgSelectOpts, Widget)
pkgSelectForm = renderDivs $ PkgSelectOpts
    <$> areq (jqueryDayField def
        { jdsChangeMonth = True
        , jdsChangeYear = True
        , jdsYearRange = "2006:c"
        }) (lbAndId "Last upload not earlier" "pkgSlctFrom")
           (Just $ fromGregorian 2006 1 1)
    <*> areq intField (lbAndId "Downloads per month at least" "pkgSlctDwnlds") (Just 1)
    <*> areq checkBoxField (lbAndId "Must be accessible on Stackage" "pkgSlctStackage") (Just False)
    <*> aopt hiddenField (idAndName "pkgSlctCategs")
            Nothing


pkgSelectedWidget:: PkgSelectOpts -> Widget
pkgSelectedWidget (PkgSelectOpts{..}) =
    let pkgViewLimit = 200
        stgCnd = if pkgSlctStackage then " and stackage_ver <> ''" else T.empty
        catCnd = case pkgSlctCategs of
                    Just (commaSplit -> l@(_:_)) ->
                        concat[
                        " in (select pkg from users_categ where cat in \
                        \(select id from categ where name in ('",
                        intercalate "','" l,
                        "')) group by pkg having count(cat)=", tshow $ length l,")"]
                    _ -> " not in (select distinct pkg from users_categ)" 
    in do
    pks <- liftHandlerT $ runDB $ rawSql (concat[ "select ?? from pckg where id ",
            catCnd, stgCnd,
            " and last_upload > ? and downloads >= ? order by lower(name)"])
            [PersistDay pkgSlctFrom, PersistInt64 (fromIntegral pkgSlctDwnlds)]  
    let pksLn = length pks
        usersCategWidget k = do
            cns <- getUsersCategNamesFromPckgId k
            [whamlet|
                $newline never
                $forall (n,isNxt) <- zipWithNxt cns
                    $if isNxt
                        \, #
                    <span .value>
                        #{n}
            |]
    $(widgetFile "selected")

getHomeR :: Handler Html
getHomeR = chkAppState $ do
    (formWidget, formEnctype) <- generateFormPost pkgSelectForm
    defaultLayout $ do
        setTitle appTitle
        topStatWidget
        $(widgetFile "homepage")
    
postHomeR :: Handler Html
postHomeR = chkAppState $ do
    ((result, formWidget), formEnctype) <- runFormPost pkgSelectForm
    defaultLayout $ do
        setTitle appTitle
        topStatWidget
        $(widgetFile "homepage")
        case result of
            FormSuccess p -> pkgSelectedWidget p
            _ -> return ()
