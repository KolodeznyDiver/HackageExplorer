{-# LANGUAGE MultiWayIf #-}
module ScanThread(scanThread) where
import ClassyPrelude hiding (lift) 
import Yesod.Core hiding (lift)
import Text.Blaze.Html.Renderer.Text
import Database.Persist hiding (get)
import Database.Persist.Sqlite hiding (get) 
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.HTTP.Simple
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.XML
import Text.XML.Lens
import Data.Time
import qualified Data.CaseInsensitive as CIn
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import qualified Data.HashMap.Strict as HM
import HackageExplorerDefs
import Foundation
import Model
import PkgPgParser
import Control.THEff
import Control.THEff.State.Strict 
import RepairHTML

data PkgBrief = PkgBrief { pkBrId :: PckgId, pkBrVer :: T.Text }

newtype NewsCntr = NewsCntr Int deriving Enum
newtype UpdateCntr = UpdateCntr Int deriving Enum
type PkgBriefHashMap = HM.HashMap T.Text PkgBrief
type CategHashMap = HM.HashMap T.Text CategId

mkEff "NewPkCntr"       ''State    ''NewsCntr           ''Lift
mkEff "UpdatePkCntr"    ''State    ''UpdateCntr         ''NewPkCntr
mkEff "Fails"           ''State    ''Int                ''UpdatePkCntr
mkEff "Pckgs"           ''State    ''PkgBriefHashMap    ''Fails 
mkEff "Categs"          ''State    ''CategHashMap       ''Pckgs 

scanThread :: App -> IO ()
scanThread (App{..}) = flip finally (atomically $ writeTVar appState AppIdle) $ do
    let topPg = hackageURL ++ hackageTopPage
        
    say1 "Base preparing ..."
    pks <- (HM.fromList . map 
                (\(Entity k (Pckg{..})) -> 
                    (pckgName,PkgBrief k pckgVer))) <$> (runDB $ selectList [] [])
    ctgs <- (HM.fromList . map 
                (\(Entity k (Categ{..})) -> 
                    (categName,k))) <$> (runDB $ selectList [] [])
                    
    say ["Downloading ", topPg, " ..."]
    m <- newManager defaultManagerSettings
    request <- parseRequest $ topPg
    response <- httpLBS $ setRequestManager m request
    case parseLBS def{psDecodeEntities = decodeHtmlEntities} $ getResponseBody response of
        Left err  -> say1 $ show err
        Right doc -> do
            let hel = named . CIn.mk
                rows = doc ^.. root ./ hel "body" ./ attributeIs "id" "content"
                        ./ entire ./ hel "tr" 
                allPkgs = rows ^.. traverse ./ hel "td" ./ hel "a" . text
                pkgs = map T.unpack $ allPkgs
                cPackages = length pkgs
                cPk = show $ cPackages
            let sLn = concat [" from ", cPk," : "] 
            (((_,cFail),UpdateCntr cUpdt),NewsCntr cNew) <- runNewPkCntr (NewsCntr 0) 
                $ runUpdatePkCntr (UpdateCntr 0) $ runFails 0 $ runPckgs pks $ runCategs ctgs $ 
                forM_ (zip pkgs [1:: Int ..]) $ \(n,i) -> do
                    let pref = [shamlet|
                        download #{i}#{sLn}
                            <span .pkgName>#{n}
                        |]
                        addPkMap k vId ver = modify (HM.insert k (PkgBrief vId ver))
                        runEffDB = lift . runDB
                    r <- lift $ handle (\e -> return $ Left $ show (e :: SomeException)) $ do
                        a <- download m n 
                        case repairHTML $ decodeUtf8With lenientDecode a of
                            Right s ->  packageInfoExtract s
                            Left l -> return $ Left l
                    case r of 
                        Left s -> lift (sayHtml [shamlet|
                            #{pref} : #
                                <span .errMsg>#{s}
                            |]) 
                            >> modify (+(1::Int))
                        Right PackageInfo{..} -> do 
---- { db corrections for one package ---------------------------------------------
                            let newPkMsg = lift (sayHtml [shamlet|
                                #{pref} : new package version #
                                    <span .newPackage>#{piVer}
                                |]) >> modify (succ :: (NewsCntr -> NewsCntr))
                            mbPB <- (HM.lookup $ T.pack n) <$> get 
                            (pId,isNewPkg) <- 
                                case mbPB of
                                    Just (PkgBrief{..}) -> do 
                                        if  | T.null pkBrVer -> newPkMsg
                                            | pkBrVer /= piVer -> 
                                                lift (sayHtml [shamlet|
                                                    #{pref} : update version #
                                                        <span .oldVersion>#{pkBrVer}
                                                    \ &#8680; #
                                                        <span .updatedPackage>#{piVer}
                                                    |]) >>
                                                modify (succ :: (UpdateCntr -> UpdateCntr))
                                            | otherwise -> lift $ sayHtml pref
                                        runEffDB $ update pkBrId [
                                                    PckgShortDescr =. piShortDesc,
                                                    PckgDescr =. piDesc,
                                                    PckgVer =. piVer,
                                                    PckgAuthor =. piAuthor,
                                                    PckgHomePage =. piHomePg,
                                                    PckgReps =. piReps,
                                                    PckgLastUpload =. piLastUp,
                                                    PckgStackageVer =. piStackageVer,
                                                    PckgStackageURL =. piStackageURL,
                                                    PckgDownloads =. piDownloads,
                                                    PckgIsDoc   =. piIsDoc
                                                    ]
                                        return (pkBrId, T.null pkBrVer)
                                    _ -> do 
                                        newPkMsg 
                                        pid <- runEffDB $ insert $ Pckg
                                                piName piShortDesc piDesc piVer piAuthor
                                                piHomePg piReps piLastUp piStackageVer
                                                piStackageURL piDownloads piIsDoc T.empty
                                        addPkMap piName pid piVer
                                        return (pid,True) 
                            runEffDB $ deleteWhere [AuthorsCategPkg ==. pId]
                            forM_ piCats $ \c -> do
                                mbc <- HM.lookup c <$> get
                                cId <- case mbc of
                                        Just ci -> return ci
                                        _ -> do
                                            ci <- runEffDB $ insert $ Categ c
                                            modify (HM.insert c ci)
                                            return ci
                                runEffDB $ void $ insert $ AuthorsCateg pId cId
                                when isNewPkg $
                                    runEffDB $ void $ insert $ UsersCateg pId cId
                            runEffDB $ deleteWhere [DependPkg ==. pId]
                            forM_ piDepends $ \d -> do
                                md <- HM.lookup d <$> get
                                dId <- case md of
                                        Just b -> return $ pkBrId b
                                        _ -> do
                                            dId' <- runEffDB $ insert $ defPckg d
                                            addPkMap d dId' T.empty
                                            return dId'
                                runEffDB $ void $ insert $ Depend pId dId
---- } end of db corrections for one package --------------------------------------
            runDB $ deleteWhere [ ScanInfoTime >.  defTime ] 
            now <- getCurrentTime
            runDB $ void $ insert $ ScanInfo now cNew cUpdt
            sayHtml [shamlet|
                <span .total>
                    Hackage rescaned. Complete : #
                        <span .value>#{cPackages - cFail}
                    , (new : #
                        <span .value>#{cNew}
                    , updated : #
                        <span .value>#{cUpdt}
                    ), fails : #
                        <span .value>#{cFail}
            |]
  where download m n = do
            request <- parseRequest $ concat[hackageURL,hackagePkgPrefix,n]
            response <- httpLBS $ setRequestManager m request
            let body = getResponseBody response
            return body
        sayText = atomically . writeTQueue appMsgQueue 
        sayLT = sayText . LT.toStrict
        say1 = sayText . T.pack
        say = say1 . concat
        sayHtml = sayLT . renderHtml
        runDB:: ReaderT SqlBackend IO a -> IO a
        runDB action = runSqlPool action appConnPool
        defPckg n = Pckg n T.empty T.empty T.empty T.empty T.empty T.empty 
                        defTime T.empty T.empty 0 False T.empty
        defTime = UTCTime (fromGregorian 1900 1 1) $ secondsToDiffTime  0
    