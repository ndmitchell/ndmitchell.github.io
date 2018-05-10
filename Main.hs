
module Main(main) where

import Control.Monad
import System.IO.Extra
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Data.Char

type URL = String

main :: IO ()
main = do
    template <- readFile' "template.html"
    metadata <- readFile' "metadata.txt"
    let downloads = unlines $
            concat (reverse $ zipWithFrom renderMetadata 1 $ reverse $ parseMetadata metadata)
    let reps = [("#{" ++ lower proj ++ "}", "<a href=\"" ++ url ++ "\">" ++ proj ++ "</a>") | (proj,url) <- projects]
    let res = replaces (("#{downloads}",downloads):reps) template
    when ("#{" `isInfixOf` res) $ error $ "Missed a replacement, " ++ take 20 (snd $ breakOn "#{" res) ++ "..."
    writeFile "index.html" res
    putStrLn "Generated to index.html"


---------------------------------------------------------------------
-- INFORMATION TABLES

projects :: [(String, URL)]
projects =
    [("Shake","https://shakebuild.com/")
    ,("Hoogle","https://hoogle.haskell.org/")
    ,("Yhc","https://wiki.haskell.org/Yhc")
    ,("Hat","https://archives.haskell.org/projects.haskell.org/hat/")] ++
    [(x, "https://github.com/ndmitchell/" ++ lower x)
    | x <- words "HLint Supero Derive Firstify Catch Uniplate NSIS Bake Hexml Weeder"]

names = map fst projects ++ ["Haskell","Hat","Windows","Pasta"]


---------------------------------------------------------------------
-- PARSING

newtype Entry = Entry {fromEntry :: [(String, String)]} deriving Show

entryRequired = words "title date text key"
entryOptional = words "paper slides video audio where author abstract"

(!) :: Entry -> String -> String
(!) xs y = fromMaybe (error $ "! failed, looking for " ++ show y ++ " in " ++ show xs) $ xs !? y

(!?) :: Entry -> String -> Maybe String
(!?) (Entry xs) y = lookup y xs


checkMetadata :: [Entry] -> [Entry]
checkMetadata xs | all (checkFields . map fst . fromEntry) xs = reverse $ sortOn (\x -> parseDate $ x ! "date") xs
    where
        checkFields xs | bad:_ <- xs \\ nub xs = error $ "Duplicate field, " ++ bad
                       | bad:_ <- filter (not . isPrefixOf "@") xs \\ (entryRequired ++ entryOptional) = error $ "Unknown field, " ++ bad
                       | bad:_ <- entryRequired \\ xs = error $ "Missing field, " ++ bad
                       | otherwise = True

parseMetadata :: String -> [Entry]
parseMetadata = checkMetadata . map (Entry . map f) . wordsBy null . rejoin . map trimEnd . lines . replace "\t" "    "
    where
        f = second (trim . drop 1) . breakOn ":"

        rejoin (x:"":(' ':y):zs) = rejoin $ (x ++ "\n" ++ trim y) : zs
        rejoin (x:(' ':y):zs) = rejoin $ (x ++ " " ++ trim y) : zs
        rejoin (x:xs) = x : rejoin xs
        rejoin [] = []


---------------------------------------------------------------------
-- RENDERING

renderMetadata :: Int -> Entry -> [String]
renderMetadata unique e =
        [""
        ,"<h3>" ++ typ ++ ": " ++ e ! "title" ++ "</h3>"
        ,"<p class=\"info\">" ++ intercalate ", " parts ++ location ++ ", " ++ coauthors ++ e ! "date" ++ ".</p>"
        ,"<p id=\"citation" ++ show unique ++ "\" class=\"citation\">" ++ renderBibtex e ++ "</p>"] ++
        ["<p id=\"abstract" ++ show unique ++ "\" class=\"abstract\"><b>Abstract:</b> " ++ replace "\n" "<br/><br/>" abstract ++ "</p>" | abstract /= ""] ++
        ["<p class=\"text\">" ++ e ! "text" ++ "</p>"]
    where
        typ | e !? "@at" == Just "phdthesis" = "Thesis"
            | isJust $ e !? "paper" = "Paper"
            | otherwise = "Talk"
        parts = [ "<a href=\"" ++ download v ++ "\">" ++ (if i == 0 then toUpper (head k) : tail k else k) ++ "</a>"
                | (i,(k,v)) <- zipFrom 0 [(k, v) | k <- words "paper slides video audio", Just v <- [e !? k]]] ++
                [ "<a href=\"javascript:showCitation(" ++ show unique ++ ")\">citation</a>"] ++
                [ "<a href=\"javascript:showAbstract(" ++ show unique ++ ")\">abstract</a>" | abstract /= ""]
        download x = if "http" `isPrefixOf` x then x else "downloads/" ++ x
        location = maybe "" (" from " ++) $ e !? "where"
        coauthors = case delete "Neil Mitchell" $ maybe [] (splitOn " and ") (e !? "author") of
            [] -> ""
            [x] -> "with " ++ x ++ ", "
            [x,y] -> "with " ++ x ++ " and " ++ y ++ ", "
            xs -> "with " ++ intercalate ", " (init xs) ++ " and " ++ last xs ++ ", "

        abstract = fromMaybe "" $ e !? "abstract"


renderBibtex :: Entry -> String
renderBibtex e = unlines $ ("@" ++ at ++ "{mitchell:" ++ key) : map showBibLine items ++ ["}"]
    where
        (at,ex) | isJust $ e !? "paper" = (fromMaybe "inproceedings" $ e !? "@at", [])
                | otherwise = ("misc",[("note","Presentation" ++ whereText)])
        items = filter (not . null . snd)
                [("title", capitalise $ e ! "title")
                ,("author", fromMaybe "Neil Mitchell" $ e !? "author")
                ,("year", show $ fst3 date)
                ,("month", show (snd3 date))
                ,("day", show $ thd3 date)
                ] ++ ex ++
                [(a,b) | ('@':a,b) <- fromEntry e, a /= "at"] ++
                [("url", "\\verb'https://ndmitchell.com/downloads/" ++ url ++ "'")
                    | url <- take 1 [e ! s | s <- ["paper", "slides"], isJust $ e !? s]]

        date = parseDate $ e ! "date"
        key = (e ! "key") ++ "_" ++ replace " " "_" (lower $ e ! "date")
        whereText = maybe "" (\x -> " from " ++ stripTags x) $ e !? "where"

showBibLine (a,b) = "    ," ++ a ++ replicate (14 - length a) ' ' ++ " = {" ++ (if a == "pages" then f b else b) ++ "}"
    where
        f (x:'-':y:xs) | isDigit x && isDigit y = x:'-':'-':y : f xs
        f (x:xs) = x : f xs
        f [] = []

-- capitalise the title in some way
capitalise :: String -> String
capitalise str = unwords (f True x : map (f False) xs)
    where
        (x:xs) = words str

        f first (x:xs) | ":" `isSuffixOf` xs = f first (x : take (length xs - 1) xs) ++ ":"
                       | (any isUpper xs && '-' `notElem` xs)
                       || (not first && (x:xs) `elem` names) = "{" ++ x:xs ++ "}"
                       | otherwise = x:xs


---------------------------------------------------------------------
-- UTILITIES

-- | Perform all the replacements
replaces :: Eq a => [([a], [a])] -> [a] -> [a]
replaces reps x = foldl (\x (from,to) -> replace from to x) x reps

-- | Remove HTML tags <foo>
stripTags ('<':xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x:xs) = x : stripTags xs
stripTags [] = []

data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show,Eq,Ord,Enum,Bounded)

parseDate :: String -> (Int, Month, Int)
parseDate x
    | [a,b,c] <- words x
    , [month] <- filter (\x -> take 3 (show x) == b) [January .. December]
    = (read c, month, read a)
