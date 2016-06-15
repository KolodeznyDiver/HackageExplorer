module Handler.UnusedCat(getUnusedCatR,postRemoveCatR) where

import Import
import Data.Aeson.Types
import Database.Persist.Sql
import HackageExplorerDefs
import Handler.Home
import AppWidgets

getUnusedCatR :: Handler Html
getUnusedCatR = chkAppState $ do
    ctgs <- runDB $ map unSingle <$> rawSql
            "select name from categ where id not in (\
            \select distinct cat from users_categ union \
            \select distinct cat from authors_categ) order by lower(name)" []
    defaultLayout $ do
        setTitle appTitle
        $(widgetFile "unusedcat")
        
newtype RemoveCatReq = RemoveCatReq { removeCatName  :: Text }
    
instance FromJSON RemoveCatReq where
  parseJSON (Object v) = RemoveCatReq <$> v .: "catNameToRemove"
  parseJSON invalid    = typeMismatch "RemoveCatReq" invalid

postRemoveCatR :: Handler Value 
postRemoveCatR  = do
    RemoveCatReq{..} <- requireJsonBody
    runDB $ deleteWhere [CategName ==. removeCatName]
    return $ object ["ok" .= True] 