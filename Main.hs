
module Main(main) where

import Control.Monad
import System.IO.Extra
import Data.Maybe
import Data.List.Extra hiding ((!?))
import Data.Tuple.Extra
import Data.Ord
import Data.Char

type URL = String

main :: IO ()
main = do
    template <- readFile' "template.html"
    metadata <- readFile' "metadata.txt"
    let downloads = unlines $ concatMap renderMetadata $ parseMetadata metadata
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
    ,("Ghcide","https://github.com/haskell/ghcide")
    ,("HLS","https://github.com/haskell/haskell-language-server")
    ,("Hat","https://archives.haskell.org/projects.haskell.org/hat/")] ++
    [(x, "https://github.com/ndmitchell/" ++ lower x)
    | x <- words "HLint Supero Derive Firstify Catch Uniplate NSIS Bake Hexml Weeder Ghcid Rattle"]

authors :: [(String, URL)]
authors =
    [("Colin Runciman","https://www-users.cs.york.ac.uk/~colin/")
    ,("Simon Peyton Jones","https://www.microsoft.com/en-us/research/people/simonpj/")
    ,("Andrey Mokhov","https://www.ncl.ac.uk/engineering/staff/profile/andreymokhov.html")
    ,("Simon Marlow","https://simonmar.github.io/")
    ,("Dimitry Golubovsky","https://github.com/dmgolubovsky")
    ,("Matthew Naylor","")
    ,("Joachim Breitner","https://www.cis.upenn.edu/~joachim/")
    ,("Brian Huffman","")
    ,("Christian Sternagel","https://cl-informatik.uibk.ac.at/users/griff/")
    ,("Moritz Kiefer","https://github.com/cocreature")
    ,("Pepe Iborra","https://github.com/pepeiborra")
    ,("Luke Lau","https://github.com/Bubba")
    ,("Zubin Duggal","https://github.com/wz1000")
    ,("Hannes Siebenhandl","") -- nothing ties his name to his profile pages, so don't start
    ,("Matthew Pickering","https://mpickering.github.io/")
    ,("Alan Zimmerman","https://github.com/alanz")
    ,("Sarah Spall", "https://github.com/spall")
    ,("Sam Tobin-Hochstadt", "https://samth.github.io/")
    ,("Javier Neira Sanchez", "https://github.com/jneira")
    ]

names = map fst projects ++ ["Haskell","Hat","Windows","Pasta"]


---------------------------------------------------------------------
-- PARSING

newtype Entry = Entry {fromEntry :: [(String, String)]} deriving Show

entryRequired = words "title date text key"
entryOptional = words "paper preprint slides video audio where author abstract"

(!) :: Entry -> String -> String
(!) xs y = fromMaybe (error $ "! failed, looking for " ++ show y ++ " in " ++ show xs) $ xs !? y

(!?) :: Entry -> String -> Maybe String
(!?) (Entry xs) y = lookup y xs


checkMetadata :: [Entry] -> [Entry]
checkMetadata xs | all (checkFields . map fst . fromEntry) xs = sortOn (\x -> Down $ parseDate $ x ! "date") xs
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

renderMetadata :: Entry -> [String]
renderMetadata e =
        [""
        ,"<h3 id=\"" ++ key ++ "\"><a href=\"#" ++ key ++ "\">" ++ typ ++ ": " ++ e ! "title" ++ "</a></h3>"
        ,"<p class=\"info\">" ++ intercalate ", " parts ++ location ++ ", " ++ coauthors ++ e ! "date" ++ ".</p>"
        ,"<p id=\"citation_" ++ key ++ "\" class=\"citation\">" ++ renderBibtex e ++ "</p>"] ++
        ["<p id=\"abstract_" ++ key ++ "\" class=\"abstract\"><b>Abstract:</b> " ++ replace "\n" "<br/><br/>" abstract ++ "</p>" | abstract /= ""] ++
        ["<p class=\"text\">" ++ e ! "text" ++ "</p>"]
    where
        key = entryKey e
        typ | e !? "@at" == Just "phdthesis" = "Thesis"
            | isJust $ e !? "paper" = "Paper"
            | isJust $ e !? "preprint" = "Preprint"
            | otherwise = "Talk"
        parts = [ "<a href=\"" ++ download v ++ "\">" ++ (if i == 0 then toUpper (head k) : tail k else k) ++ "</a>"
                | (i,(k,v)) <- zipFrom 0 [(k, v) | k <- words "paper preprint slides video audio", Just v <- [e !? k]]] ++
                [ "<a href=\"javascript:showCitation(\'" ++ key ++ "\')\">citation</a>"] ++
                [ "<a href=\"javascript:showAbstract(\'" ++ key ++ "\')\">abstract</a>" | abstract /= ""]
        download x = if "http" `isPrefixOf` x then x else "downloads/" ++ x
        location = maybe "" (" from " ++) $ e !? "where"
        coauthors = case map author $ delete "Neil Mitchell" $ maybe [] (splitOn " and ") (e !? "author") of
            [] -> ""
            [x] -> "with " ++ x ++ ", "
            [x,y] -> "with " ++ x ++ " and " ++ y ++ ", "
            xs -> "with " ++ intercalate ", " (init xs) ++ " and " ++ last xs ++ ", "
        author x = if url == "" then x else "<a class=\"author\" href=\"" ++ url ++ "\">" ++ x ++ "</a>"
            where url = fromMaybe (error $ "No link for " ++ show x) $ lookup x authors

        abstract = fromMaybe "" $ e !? "abstract"


renderBibtex :: Entry -> String
renderBibtex e = unlines $ ("@" ++ at ++ "{mitchell:" ++ entryKey e) : map showBibLine items ++ ["}"]
    where
        (at,ex) | isJust $ e !? "paper" = (fromMaybe "inproceedings" $ e !? "@at", [])
                | isJust $ e !? "preprint" = ("unpublished",[("note","Preprint")])
                | otherwise = ("misc",[("note","Presentation" ++ whereText)])
        items = map (second htmlEscapeToLatex) $ filter (not . null . snd)
                [("title", capitalise $ e ! "title")
                ,("author", fromMaybe "Neil Mitchell" $ e !? "author")
                ,("year", show $ fst3 date)
                ,("month", show (snd3 date))
                ,("day", show $ thd3 date)
                ] ++ ex ++
                [(a,b) | ('@':a,b) <- fromEntry e, a /= "at"] ++
                [("url", (if isUrlAbsolute url then "" else "https://ndmitchell.com/downloads/") ++ url)
                    | url <- take 1 [e ! s | s <- ["paper", "slides"], isJust $ e !? s]]

        date = parseDate $ e ! "date"
        whereText = maybe "" (\x -> " from " ++ stripTags x) $ e !? "where"

showBibLine (a,b) = "    ," ++ a ++ replicate (14 - length a) ' ' ++ " = {" ++ (if a == "pages" then f b else b) ++ "}"
    where
        f (x:'-':y:xs) | isDigit x && isDigit y = x:'-':'-':y : f xs
        f (x:xs) = x : f xs
        f [] = []

htmlEscapeToLatex :: String -> String
htmlEscapeToLatex = replace "&agrave;" "\\`a"

entryKey :: Entry -> String
entryKey e = takeWhile isAlpha (e ! "key") ++ "_" ++ replace " " "_" (lower $ e ! "date")

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

isUrlAbsolute :: String -> Bool
isUrlAbsolute x = any (`isPrefixOf` x) ["http:","https:"]

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
    = if length a /= 2 then error $ "Dates should have a 2 digit day: " ++ show x
      else (read c, month, read a)
