module HackageExplorerAux where

import Import
import Data.Text (split,strip)

sortCIText :: [Text] -> [Text]
sortCIText = sortBy (compare `on` toLower)
 
boolToHtml :: Bool -> Html
boolToHtml False = "No"
boolToHtml _     = "Yes"

nxtList:: [Bool]
nxtList = False:repeat True

zipWithNxt:: [a] -> [(a,Bool)]
zipWithNxt l = zip l nxtList

time2Str:: UTCTime -> String
time2Str = formatTime defaultTimeLocale "%c" 

commaSplit :: Text -> [Text] 
commaSplit = filter (not . null) . split (','==) . strip

lbAndId :: SomeMessage App -> Text -> FieldSettings App
lbAndId l i = FieldSettings
    { fsLabel = l
    , fsTooltip = Nothing
    , fsId = Just i
    , fsName = Just i
    , fsAttrs = []
    }

idAndName :: Text -> FieldSettings App
idAndName i  = FieldSettings
    { fsLabel = ""
    , fsTooltip = Nothing
    , fsId = Just i
    , fsName = Just i
    , fsAttrs = []
    }
