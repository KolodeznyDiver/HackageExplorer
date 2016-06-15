module HackageExplorerDefs where

import ClassyPrelude
import Yesod.Core

data AppState = AppIdle | AppScan
                deriving Eq

appTitle :: Html; appTitle = "HackageExplorer. hackage.haskell.org packages scaner and local explorer"
hackageURL :: String; hackageURL = "http://hackage.haskell.org"
hackageTopPage  :: String; hackageTopPage = "/packages/top"
hackagePkgPrefix   :: String; hackagePkgPrefix = "/package/"
