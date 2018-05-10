
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

type Entry = [(String, String)]

entryRequired = words "title date text key"
entryOptional = words "paper slides video audio where author abstract"

(!) :: Entry -> String -> String
(!) xs y = fromMaybe (error $ "! failed, looking for " ++ show y ++ " in " ++ show (map fst xs)) $ xs !? y

(!?) :: Entry -> String -> Maybe String
(!?) xs y = lookup y xs


checkMetadata :: [Entry] -> [Entry]
checkMetadata xs | all (checkFields . map fst) xs = reverse $ sortOn (\x -> parseDate $ x ! "date") xs
    where
        checkFields xs | bad:_ <- xs \\ nub xs = error $ "Duplicate field, " ++ bad
                       | bad:_ <- filter (not . isPrefixOf "@") xs \\ (entryRequired ++ entryOptional) = error $ "Unknown field, " ++ bad
                       | bad:_ <- entryRequired \\ xs = error $ "Missing field, " ++ bad
                       | otherwise = True

parseMetadata :: String -> [Entry]
parseMetadata = checkMetadata . map (map f) . wordsBy null . rejoin . map trimEnd . lines . replace "\t" "    "
    where
        f = second (trim . drop 1) . breakOn ":"

        rejoin (x:"":(' ':y):zs) = rejoin $ (x ++ "\n" ++ trim y) : zs
        rejoin (x:(' ':y):zs) = rejoin $ (x ++ " " ++ trim y) : zs
        rejoin (x:xs) = x : rejoin xs
        rejoin [] = []


---------------------------------------------------------------------
-- RENDERING

renderMetadata :: Int -> Entry -> [String]
renderMetadata unique xs =
        [""
        ,"<h3>" ++ typ ++ ": " ++ at "title" ++ "</h3>"
        ,"<p class=\"info\">" ++ intercalate ", " parts ++ (if null $ at "where" then "" else " from " ++ at "where") ++ ", " ++ at "date" ++ ".</p>"
        ,"<p id=\"citation" ++ show unique ++ "\" class=\"citation\">" ++ bibtex xs ++ "</p>"] ++
        ["<p id=\"abstract" ++ show unique ++ "\" class=\"abstract\"><b>Abstract:</b> " ++ replace "\n" "<br/><br/>" abstract ++ "</p>" | abstract /= ""] ++
        ["<p class=\"text\">" ++ at "text" ++ "</p>"]
    where
        typ | ("@at","phdthesis") `elem` xs = "Thesis"
            | "paper" `elem` keys = "Paper"
            | otherwise = "Talk"
        parts = [ "<a href=\"" ++ download v ++ "\">" ++ (if i == 0 then toUpper (head k) : tail k else k) ++ "</a>"
                | (i,(k,v)) <- zip [0..] $ filter (not . null . snd) $ map (id &&& at) $ words "paper slides video audio"] ++
                [ "<a href=\"javascript:showCitation(" ++ show unique ++ ")\">citation</a>"] ++
                [ "<a href=\"javascript:showAbstract(" ++ show unique ++ ")\">abstract</a>" | abstract /= ""]
        download x = if "http" `isPrefixOf` x then x else "downloads/" ++ x

        at x = unwords $ map snd $ filter ((==) x . fst) xs
        abstract = at "abstract"
        keys = map fst xs


bibtex :: Entry -> String
bibtex x = unlines $ ("@" ++ at ++ "{mitchell:" ++ key) : map showBibLine items ++ ["}"]
    where
        (at,ex) | "paper" `elem` map fst x = (fromMaybe "inproceedings" $ x !? "@at", [])
                | otherwise = ("misc",[("note","Presentation" ++ whereText)])
        items = filter (not . null . snd)
                [("title", capitalise $ x ! "title")
                ,("author", fromMaybe "Neil Mitchell" $ x !? "author")
                ,("year", show $ fst3 date)
                ,("month", show (snd3 date))
                ,("day", show $ thd3 date)
                ] ++ ex ++
                [(a,b) | ('@':a,b) <- x, a /= "at"] ++
                [("url", "\\verb'https://ndmitchell.com/downloads/" ++ url ++ "'")
                    | url <- take 1 [x ! s | s <- ["paper", "slides"], isJust $ x !? s]]

        date = parseDate $ x ! "date"
        key = (x ! "key") ++ "_" ++ replace " " "_" (lower $ x ! "date")
        whereText = maybe "" (\x -> " from " ++ stripTags x) $ x !? "where"

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
