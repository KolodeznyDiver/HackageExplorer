module RepairHTML(repairHTML) where

import Prelude
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import Data.Monoid
import Text.Parsec
import Data.Char(toUpper)
import Control.Monad (void)

type HTMLParse' a = Parsec L.Text () a
type HTMLParse = HTMLParse' Builder

repairHTML :: L.Text -> Either String L.Text
repairHTML = either (Left . show) Right . parse parseDoc "" where
    parseDoc :: HTMLParse' L.Text
    parseDoc = do
        skipMany space
        sDoc <- docTag <|> return "<!DOCTYPE html>"
        h <- parseHTML
        return $ toLazyText $ sDoc <> h
    fromStr2 l r = fromString l <> fromString r
    cons c b = singleton c <> b
    snoc b c = b <> singleton c
    parseHTML :: HTMLParse
    parseHTML = mconcat <$> many (parseTag <|> (fromString <$> many1 (noneOf "<")) ) 
    parseTag :: HTMLParse
    parseTag = do
        void $ char '<'
        i <- parseHTMLComment <|> (do
            j <- closeTag <|> openTag 
            void $ char '>'
            return $ snoc j '>'
                                  )
        return $ cons '<' i
    openTag :: HTMLParse
    openTag = do
        skipSpace
        tN <- nameLC
        i <- mconcat <$> many ((many1 space >> return " ") <|> attr)
        c <- optionMaybe $ char '/'
        skipSpace
        c' <- case c of
                Nothing 
                    | tN `elem` ["area","base","basefont","bgsound","br","col",
                        "command","embed","hr","img","input","isindex",
                        "keygen","link","meta","param","source","track","wbr"]
                        -> return "/"
                    | otherwise -> 
                        if tN `elem` ["script","style"] 
                        then beforeStr $ L.unpack $ L.append "</" tN 
                        else return ""
                _ -> return "/"
        return $ fromLazyText tN <> i <> c'
    attr :: HTMLParse
    attr = do
        n <- nameLCB
        skipSpace
        r <- option (bAround '\"' n) $ do
            void $ char '='
            skipSpace
            qStr <|> (many1 (noneOf " \t\n\r>") >>= sAroundM '\"') 
        return $ n <> cons '=' r          
    closeTag :: HTMLParse
    closeTag = do
        void $ char '/'
        skipSpace
        i <- nameLCB
        skipMany noGT
        return $ cons '/' i
    bAround c b = let s = singleton c in s <> b <> s
--    bAroundM c b = return $ bAround c b
--    tAround c t = bAround c $ fromLazyText t
--    tAroundM c t = return $ tAround c t
    sAround c t = bAround c $ fromString t
    sAroundM c t = return $ sAround c t
    qStr' :: Char -> HTMLParse 
    qStr' c = do
        i <- between (char c) (char c) (many (noneOf [c]))
        sAroundM c i 
    qStr :: HTMLParse 
    qStr = qStr' '\"' <|> qStr' '\''
    beforeStr s = do
        i <- manyTill anyChar (try (stringCI s))
        return $ fromStr2 i s
    parseHTMLComment :: HTMLParse    
    parseHTMLComment = (<>) "!--" <$> (string "!--" >> beforeStr "-->")
    docTag :: HTMLParse
    docTag = do
        h <- string "<!DOCTYPE"
        skipMany1 space
        i <- many noGT 
        void $ char '>'
        return $ snoc (fromString h) ' ' <> snoc (fromString i) '>'
    noGT :: HTMLParse' Char 
    noGT = noneOf ">"
    skipSpace :: HTMLParse' ()
    skipSpace = skipMany space
    nameLC :: HTMLParse' L.Text
    nameLC = do
        c  <- letter
        cs <- many alphaNum
        return $ L.toLower $ L.pack $ c:cs
    nameLCB :: HTMLParse
    nameLCB = fromLazyText <$> nameLC
    charCI :: Char -> HTMLParse' Char
    charCI c = char c <|> char (toUpper c)
    stringCI :: String -> HTMLParse
    stringCI s = fromString <$> try (mapM charCI s)

