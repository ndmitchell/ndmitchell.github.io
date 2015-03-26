{-# LANGUAGE ViewPatterns #-}

module Main(main) where

import Control.Monad
import System.IO.Extra
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Data.Char


main :: IO ()
main = do
    template <- readFile' "template.html"
    metadata <- readFile' "metadata.txt"
    let download = unlines $
            ("<p>All papers and talks are listed below, most recent first. " ++
             "Show all <a href=\"javascript:showAbstracts()\">abstracts</a> or " ++
             "<a href=\"javascript:showCitations()\">citations</a>.</p>") :
            concat (reverse $ zipWith renderMetadata [1..] $ reverse $ checkMetadata $ parseMetadata metadata)
    let reps = [("#{" ++ lower proj ++ "}", "<a href=\"" ++ url ++ "\">" ++ proj ++ "</a>") | (proj,url) <- projects]
    let res = replaces (("#{download}",download):reps) template
    when ("#{" `isInfixOf` res) $ error $ "Missed a replacement, " ++ take 20 (snd $ breakOn "#{" res) ++ "..."
    writeFile "index.html" res
    putStrLn "Generated to index.html"

projects :: [(String, String)]
projects =
    [("Shake","http://shakebuild.com/")
    ,("Hoogle","http://hoogle.haskell.org/")
    ,("Yhc","https://wiki.haskell.org/Yhc")
    ,("Hat","http://projects.haskell.org/hat/")] ++
    [(x, "https://github.com/ndmitchell/" ++ lower x)
    | x <- words "HLint Supero Derive Firstify Catch Uniplate NSIS Bake"]

replaces :: Eq a => [([a], [a])] -> [a] -> [a]
replaces reps x = foldl (\x (from,to) -> replace from to x) x reps

renderMetadata :: Int -> [(String, String)] -> [String]
renderMetadata unique xs =
        [""
        ,"<h3>" ++ typ ++ ": " ++ at "title" ++ "</h3>"
        ,"<p class=\"info\">" ++ intercalate ", " parts ++ (if null $ at "where" then "" else " from " ++ at "where") ++ ", " ++ at "date" ++ ".</p>"
        ,"<p id=\"citation" ++ show unique ++ "\" class=\"citation\">" ++ bibtex xs ++ "</p>"] ++
        ["<p id=\"abstract" ++ show unique ++ "\" class=\"abstract\"><b>Abstract:</b> " ++ replace "\n" "<br/><br/>" abstract ++ "</p>" | abstract /= ""] ++
        ["<p class=\"text\">" ++ at "text" ++ "</p>"]
    where
        typ = if ("@at","phdthesis") `elem` xs then "Thesis"
              else if "paper" `elem` keys then "Paper"
              else if "slides" `elem` keys then "Talk"
              else "Other"
        parts = [ "<a href=\"" ++ download v ++ "\">" ++ (if i == 0 then toUpper (head k) : tail k else k) ++ "</a>"
                | (i,(k,v)) <- zip [0..] $ filter (not . null . snd) $ map (id &&& at) $ words "paper slides video audio"] ++
                [ "<a href=\"javascript:showCitation(" ++ show unique ++ ")\">citation</a>"] ++
                [ "<a href=\"javascript:showAbstract(" ++ show unique ++ ")\">abstract</a>" | abstract /= ""]
        download x = if "http" `isPrefixOf` x then x else "downloads/" ++ x

        at x = unwords $ map snd $ filter ((==) x . fst) xs
        abstract = at "abstract"
        keys = map fst xs


checkMetadata :: [[(String, String)]] -> [[(String, String)]]
checkMetadata xs | all (checkFields . map fst) xs = reverse $ sortOn date xs
    where
        checkFields xs | bad:_ <- xs \\ nub xs = error $ "Duplicate field, " ++ bad
                       | bad:_ <- filter (not . isPrefixOf "@") xs \\ (required ++ optional) = error $ "Unknown field, " ++ bad
                       | bad:_ <- required \\ xs = error $ "Missing field, " ++ bad
                       | otherwise = True
        date = parseDate . fromJust . lookup "date"

        required = words "title date text key"
        optional = words "paper slides video audio where author abstract"


parseDate :: String -> (Int, Int, Int)
parseDate x | [a,b,c] <- words x = (read c, 1 + fromMaybe (error $ "bad month, " ++ x) (elemIndex b months), read a)
    where months = words "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"


parseMetadata :: String -> [[(String, String)]]
parseMetadata = map (map f) . wordsBy null . rejoin . map trimEnd . lines . replace "\t" "    "
    where
        f = second (trim . drop 1) . breakOn ":"

        rejoin (x:"":(' ':y):zs) = rejoin $ (x ++ "\n" ++ trim y) : zs
        rejoin (x:(' ':y):zs) = rejoin $ (x ++ " " ++ trim y) : zs
        rejoin (x:xs) = x : rejoin xs
        rejoin [] = []



bibtex :: [(String, String)] -> String
bibtex x = unlines $ ("@" ++ at ++ "{mitchell:" ++ key) : map showBibLine items ++ ["}"]
    where
        (at,ex) | "paper" `elem` map fst x = (fromMaybe "inproceedings" $ lookup "@at" x, [])
                | otherwise = ("misc",[("note","Presentation" ++ whereText)])
        items = filter (not . null . snd)
                [("title", capitalise $ x !# "title")
                ,("author", fromMaybe "Neil Mitchell" $ lookup "author" x)
                ,("year", show $ fst3 date)
                ,("month", months !! (snd3 date - 1))
                ,("day", show $ thd3 date)
                ] ++ ex ++
                [(a,b) | ('@':a,b) <- x, a /= "at"] ++
                [("url", "\\verb'http://ndmitchell.com/downloads/" ++ (if x !? "paper" then x !# "paper" else x !# "slides") ++ "'")]

        date = parseDate $ x !# "date"
        key = (x !# "key") ++ "_" ++ replace " " "_" (lower $ x !# "date")
        whereText = maybe [] (\x -> " from " ++ stripTags x) $ lookup "where" x

stripTags ('<':xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x:xs) = x : stripTags xs
stripTags [] = []

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

names = ["Haskell","Uniplate","Hat","Windows","Pasta"]

(!#) :: [(String,a)] -> String -> a
(!#) xs y = fromMaybe (error $ "!# failed, looking for " ++ show y ++ " in " ++ show (map fst xs)) $
            lookup y xs

(!?) :: [(String,a)] -> String -> Bool
(!?) xs y = y `elem` map fst xs


months = ["January","February","March","April","May","June"
         ,"July","August","September","October","November","December"]

