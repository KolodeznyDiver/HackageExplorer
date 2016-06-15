module Handler.PkgView(getPkgViewR,postNewCategR,postUpdateCategR
                      ,postUpdateCommentR) where

import Import
import Data.Aeson.Types
import Database.Persist.Sql
import HackageExplorerDefs
import HackageExplorerAux
import Handler.Home
import AppWidgets
import qualified Data.List as L

getPkgViewR :: Text -> Handler Html
getPkgViewR pkgname = chkAppState $ do
    pkgLst <- runDB $ selectList [PckgName ==. pkgname] []
    case pkgLst of
        [e@(Entity k v)] -> do
            let dep sql = runDB $ rawSql sql [L.head $ keyToValues k]
            dpnds <- dep
                "select name from pckg where id in \
                \(select \"on\" from depend where pkg=?) order by lower(name)"
            usedby <- dep
                "select name from pckg where id in \
                \(select pkg from depend where \"on\"=?) order by lower(name)"
            defaultLayout $ do
                setTitle appTitle
                cns <- getUsersCategNamesFromPckgId k
                $(widgetFile "pkgview")
        _ -> notFound

data PkgUpdtReq = PkgUpdtReq 
    { packageId :: PckgId
    , textData  :: Text
    }
    
instance FromJSON PkgUpdtReq where
  parseJSON (Object v) = PkgUpdtReq <$> v .: "id" <*> v .: "s"
  parseJSON invalid    = typeMismatch "PkgUpdtReq" invalid

        
postNewCategR :: Handler Value 
postNewCategR  = do
    PkgUpdtReq{..} <- requireJsonBody
    runDB $ do
        cId <- insert $ Categ textData
        insert_ $ UsersCateg packageId cId
    return $ object ["ok" .= True] 

postUpdateCategR :: Handler Value 
postUpdateCategR  = do
    PkgUpdtReq{..} <- requireJsonBody
    runDB $ do
        deleteWhere [UsersCategPkg ==. packageId]
        l <- selectKeysList [ CategName <-. commaSplit textData ] []
        insertMany_ $ map (UsersCateg packageId) l 
    return $ object ["ok" .= True] 

postUpdateCommentR :: Handler Value 
postUpdateCommentR  = do
    PkgUpdtReq{..} <- requireJsonBody
    runDB $ update packageId [PckgComment =. textData]
    return $ object ["ok" .= True] 