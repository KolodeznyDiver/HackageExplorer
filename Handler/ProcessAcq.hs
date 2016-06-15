module Handler.ProcessAcq where

import Import
import Control.Monad.Extra
-- import Control.Monad.STM
-- import Control.Concurrent.STM.TVar
-- import Control.Concurrent.STM.TQueue
import qualified Control.Monad.Loops as ML
import HackageExplorerDefs

postProcessAcqR :: Handler Value
postProcessAcqR  = do
    App{..} <- getYesod
    (st,msgs) <- atomicallyM $ (,) <$> readTVar appState
                                   <*> ML.unfoldM (tryReadTQueue appMsgQueue)
    return $ object ["finished" .= (st==AppIdle), "msgs" .= msgs]
{-
postCommentR :: Handler Value
postCommentR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    comment <- (requireJsonBody :: Handler Comment)

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    maybeCurrentUserId <- maybeAuthId
    let comment' = comment { commentUserId = maybeCurrentUserId }

    insertedComment <- runDB $ insertEntity comment'
    returnJson insertedComment
-}