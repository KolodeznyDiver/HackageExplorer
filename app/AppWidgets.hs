{-# LANGUAGE ScopedTypeVariables #-} 
module AppWidgets(getCategNamesFromCategIds, getUsersCategNamesFromPckgId
                 ,categWidget,topStatWidget,pkgInfoLine1Widget
                 ,pkgInfoLine2Widget,btnWidget,getSortedCateg
                 ,categoryNameListWidget,categoryListWidget
                 ,commaSeparatedCatListWidget) where

import Import
import qualified Data.Text as T
import HackageExplorerDefs
import HackageExplorerAux

getCategNamesFromCategIds :: [CategId] -> WidgetT App IO [Text]
getCategNamesFromCategIds cIds = getSortedCateg [ CategId <-. cIds]
    
    
getUsersCategNamesFromPckgId :: PckgId -> WidgetT App IO [Text]
getUsersCategNamesFromPckgId k = do
    cs <- liftHandlerT $ runDB $ selectList [ UsersCategPkg ==. k] []
    let cIds = map (usersCategCat . entityVal) cs
    getCategNamesFromCategIds cIds

    
categWidget:: Bool -> Widget
categWidget categRemark = do
    let ctgFltr = []
    $(widgetFile "category")

topStatWidget:: Widget
topStatWidget = do
    allPkgs <- liftHandlerT $ runDB $ count [ PckgVer !=. T.empty]
    scanInf :: [Entity ScanInfo] <- liftHandlerT $ runDB $ selectList [] []
    $(widgetFile "topinfo")
    
pkgInfoLine1Widget:: Entity Pckg -> Widget
pkgInfoLine1Widget (Entity _ v) = 
    $(widgetFile "pkgInfoLine1")

pkgInfoLine2Widget:: Entity Pckg -> Widget
pkgInfoLine2Widget (Entity k v) = do
    acs <- liftHandlerT $ runDB $ selectList [ AuthorsCategPkg ==. k] []
    let acIds = map (authorsCategCat . entityVal) acs
    acns <- getCategNamesFromCategIds acIds
    $(widgetFile "pkgInfoLine2")

btnWidget:: Text -> Text -> Widget
btnWidget btnId caption = $(widgetFile "btn")

getSortedCateg :: [Filter Categ] -> WidgetT App IO [Text]
getSortedCateg  fltr = do
    l <- liftHandlerT $ runDB $ selectList fltr []
    return $ sortCIText $ map (categName . entityVal) l

categoryNameListWidget:: [Text] -> Widget
categoryNameListWidget ctgs = $(widgetFile "categoryList") 
    
categoryListWidget:: [Filter Categ] -> Widget
categoryListWidget fltr = getSortedCateg fltr >>= categoryNameListWidget

commaSeparatedCatListWidget:: [Text] -> Widget
commaSeparatedCatListWidget pks = $(widgetFile "commaseparatedpkg") 
