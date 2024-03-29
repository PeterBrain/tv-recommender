-- Peter Löcker | Viktoria Krusch

--module Main where
import Data.Char
import Data.List

import System.IO
import System.IO.Error -- error handling
import System.Directory -- rename/delte File

import Control.Exception -- error handling global
import Control.Exception.Base -- error handling global

import Text.Printf
import Text.HTML.TagSoup -- html/xml parsing

import Network.HTTP.Conduit

import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Parallel.Strategies


handler_file_not_found :: IOError -> IO ()
handler_file_not_found e = do
  putStrLn "You do not have any favorite actors yet. Please use 'add actor' to add one.\n"

handler_actor_not_found :: IOError -> IO ()
handler_actor_not_found e = do
  putStrLn "Please enter a name of an actor!\n"


data TvProgramEntry = TvProgramEntry {
  number :: Int, -- the channel number (ranging from 1 to length of the program list)
  time :: String, -- the broadcasting time
  station :: String, -- the station name
  title :: String, -- the title of the broadcast followed by
  genre :: String, -- its genre (if available)
  description :: String,
  actors :: [String]} deriving (Show, Read, Eq)


main :: IO ()
main = do
  tvListe <- getTvProgram
  loop tvListe
  putStrLn "Bye!"


loop :: [TvProgramEntry] -> IO ()
loop tvList = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  command <- words <$> getLine

  let newCommand =
       if head command == "show" then
        head command
       else
        unwords $ take 2 command

  case newCommand of
    "list" -> do
      mapM_ printTvProgram tvList
      loop tvList
    "show" -> do
      let indexShow = indexParse command where
          indexParse (h:t)
            | null t = -1 -- in case no value is given
            | (not $ all isDigit (unwords t)) = -1 -- value is not an integer
            | otherwise = (read $ unwords $ tail command)

      if indexShow > length tvList || indexShow <= 0 then -- check if value smaller than lower, higher than upper bound or zero
        putStrLn $ "Please enter a value between 1 and " ++ show (length tvList) ++ "!"
      else
        showEntry (tvList !! (indexShow - 1))

      loop tvList
    "add actor" -> do
      let actorName = unwords $ tail $ tail command
      if actorName == "" then
        handler_actor_not_found (userError "")
      else
        actorAdd actorName

      loop tvList
    "list actors" -> do
      actorList
      loop tvList
    "delete actor" -> do
      let actorName = unwords $ tail $ tail command
      if actorName == "" then
        handler_file_not_found (userError "")
      else
        actorDel actorName

      loop tvList
    "recommend" -> do
      recommend tvList
      loop tvList
    "exit" -> return ()
    "help" -> do
      help
      loop tvList
    _ -> do
      putStrLn $ "Command '" ++ unwords command ++ "' is unknown!"
      loop tvList


help :: IO ()
help = do
  putStrLn "\nThis Programm supports the following commands:"
  putStrLn "\t'list' ... shows an overview of all broadcasts"
  putStrLn "\t'show' n ... shows the details of the n-th entry in the programm list"
  putStrLn "\t'add actor' name ... adds the given name to your list of favorite actors"
  putStrLn "\t'list actors' ... shows a list of all your favorite actors"
  putStrLn "\t'delete actor' name ... removes the given name from your list of favorite actors"
  putStrLn "\t'recommend' ... shows a list of broadcast featuring your favorite actors"
  putStrLn "\t'exit' ... terminate the application"
  putStrLn "\t'help' ... shows this message"


parseDescription [] = [""]
parseDescription list = take 1 $ map fromTagText
  (filter isTagText $ takeWhile (~/= TagOpen "" [("class", "action-buttons-left")]) $ head list)


parseActor [] = [""]
parseActor [_] = [""]
parseActor list = fmap (head . take 1 . map fromTagText . filter isTagText)
  (partitions (~== "<div class=actor>") (list !! 1))


getDetails :: [String] -> IO [(String, [String])]
getDetails = linkRec [] where
  linkRec acc [] = return acc
  linkRec acc (h:t) = do
    detailsContent <- parseTags . L8.unpack <$> simpleHttp h

    let splitDetails = fmap (takeWhile (~/= TagOpen "" [("class", "border")])) $
          partitions (~== "<div class=read-more>") $
          dropWhile (~/= TagOpen "" [("class", "long-text")]) detailsContent

    let actors = parseActor splitDetails
    let description = parseDescription splitDetails

    let descriptionTuple = zip description [actors]
    linkRec (head descriptionTuple : acc) t


getTvProgram :: IO ([TvProgramEntry])
getTvProgram = do
  putStrLn "Loading Programm ..."
  content <- parseTags . L8.unpack <$> simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&format=raw"

  let parsedTele =
        fmap (
          fmap (filter (/= '\t') . filter (/= '\n') . fromTagText) .
          (filter isTagText .
          takeWhile (~/= TagOpen "" [("class", "watchlist add")]) .
          dropWhile (~/= TagOpen "" [("class", "bc-content-container")]))
        ) $ partitions (~== "<div class=bc-item>") content

  let listOfPrograms = map (filter (/= "")) parsedTele

  let linkList =
        fmap (
          ("https://www.tele.at" ++) . (fromAttrib "href" . (!! 1) . filter isTagOpen .
          takeWhile (~/= TagClose "img") . dropWhile (~/= TagOpen "div" [("class", "image")]))
        ) (partitions (~== "<div class=bc-item>") content)

  parsedDetails <- getDetails linkList-- `using` parListChunk 10 rseq
  let details = reverse parsedDetails

  let description = map fst details
  let actors = map snd details

  let entriesTEMP = insertData listOfPrograms description actors [1..]
  let tvProgramSorted = quickSortTvProgramOn station entriesTEMP
  let entries = insertDataFinal tvProgramSorted [1..]

  putStrLn "Got Content! Reading broadcast details ..."
  return entries


insertData :: [[String]] -> [String] -> [[String]] -> [Int] -> [TvProgramEntry]
insertData [] _ _ _ = []
insertData _ [] _ _ = []
insertData _ _ [] _ = []
insertData _ _ _ [] = [] -- you can't reach this expression with an infinite list
insertData (h1:t1) (h2:t2) (h3:t3) (h4:t4) =
  convertEntry h1 h2 h3 h4 : insertData t1 t2 t3 t4 where
    convertEntry x y z num = TvProgramEntry (num) (x !! 1) (x !! 2) (x !! 3) (genre x) y z where
      genre x
        | length x >= 6 = (x !! 5) -- skip episode title
        | length x == 5 = (x !! 4) -- skip nothing
        | otherwise = ""           -- skip genre


insertDataFinal :: [TvProgramEntry] -> [Int] -> [TvProgramEntry]
insertDataFinal [] _ = []
insertDataFinal _ [] = [] -- you can't reach this expression with an infinite list
insertDataFinal (h1:t1) (h2:t2) =
  changeNum h1 h2 : insertDataFinal t1 t2 where
    changeNum x num = TvProgramEntry (num) (time x) (station x) (title x) (genre x) (description x) (actors x)


recommend :: [TvProgramEntry] -> IO ()
recommend tvList = do
  let outFile = "actors.txt"
  fileContent <- lines <$> readFile outFile

  let actors = map Set.fromList (actorListRecommend tvList)
  let contentList = Set.fromList fileContent
  let result = map (Set.toList . Set.intersection contentList) actors
  let zippedResult = filterRecommend $ zip tvList (map (intercalate ", ") result)

  if zippedResult == [] then
    putStrLn "There are no recommendations for you today"
  else
    mapM_ printRecommend zippedResult

  `catch` handler_file_not_found


filterRecommend :: [(TvProgramEntry,String)] -> [(TvProgramEntry,String)]
filterRecommend [] = []
filterRecommend (h:t)
  | snd h /= "" = h : filterRecommend t
  | otherwise = filterRecommend t


actorListRecommend :: [TvProgramEntry] -> [[String]]
actorListRecommend [] = []
actorListRecommend (h:t) = getAct h : actorListRecommend t where
  getAct program = actors program


printRecommend :: (TvProgramEntry,String) -> IO ()
printRecommend (program,actors) = printf "%03i. %s %-20s %s (%s %s), %s\n"
  (number program) (time program) (station program) (title program) "featuring:" (actors) (genre program)


printTvProgram :: TvProgramEntry -> IO ()
printTvProgram program = printf "%03i. %s %-20s %s, %s\n"
  (number program) (time program) (station program) (title program) (genre program)


showEntry :: TvProgramEntry -> IO ()
showEntry program = printf "\n%s %s (%s)\n%s %s\n\n%s\n\n%s\n%s\n"
  "Title:" (title program) (genre program)
  (time program) (station program)
  (description program)
  "Actors:" (printActors (actors program)) where
    printActors [""] = ""
    printActors list = unlines $ map (\x -> "\t- " ++ x) list


actorAdd :: String -> IO ()
actorAdd name = do
  let
    outFile = "actors.txt"
    tempFile = "actors.tmp"

  fileExist <- doesFileExist outFile
  if not fileExist then writeFile outFile "" else return ()

  handle <- openFile outFile ReadMode
  fileContent <- hGetContents handle

  let append = unlines $ Set.toList $ Set.insert name (Set.fromList $ lines fileContent)

  writeFile tempFile append
  hClose handle
  removeFile outFile
  renameFile tempFile outFile


actorDel :: String -> IO ()
actorDel name = do
  let
    outFile = "actors.txt"
    tempFile = "actors.tmp"

  handle <- openFile outFile ReadMode
  fileContent <- hGetContents handle

  let filterFile = unlines $ Set.toList $ Set.delete name (Set.fromList $ lines fileContent)

  writeFile tempFile filterFile
  hClose handle
  removeFile outFile
  renameFile tempFile outFile

  `catch` handler_file_not_found


actorList :: IO ()
actorList = do
  let outFile = "actors.txt"

  handle <- openFile outFile ReadMode
  fileContent <- hGetContents handle

  if fileContent == "" then
    handler_file_not_found (userError "")
  else
    putStrLn $ "\n" ++ (unlines $ quickSortActor $ lines fileContent)

  hClose handle

  `catch` handler_file_not_found


quickSortActor :: [String] -> [String]
quickSortActor [] = []
quickSortActor [h] = [h]
quickSortActor (h:t) =
  (quickSortActor less) ++ (h : equal) ++ (quickSortActor greater) where
    less = filter (\element -> insensitive element h == LT) t -- for descending sort: (h element)
    equal = filter (\element -> insensitive element h == EQ) t
    greater = filter (\element -> insensitive element h == GT) t
    insensitive a b = compare (map toLower a) (map toLower b)


--quickSortTvProgramOn :: Ord a => (String->TvProgramEntry->a) -> [TvProgramEntry] -> [TvProgramEntry]
quickSortTvProgramOn _ [] = []
quickSortTvProgramOn _ [h] = [h]
quickSortTvProgramOn onWhat (h:t) =
  (quickSortTvProgramOn onWhat less) ++ (h : equal) ++ (quickSortTvProgramOn onWhat greater) where
    less = filter (\element -> insensitive (gettvStation onWhat element) (gettvStation onWhat h) == LT) t
    equal = filter (\element -> insensitive (gettvStation onWhat element) (gettvStation onWhat h) == EQ) t
    greater = filter (\element -> insensitive (gettvStation onWhat element) (gettvStation onWhat h) == GT) t
    insensitive a b = compare (map toLower a) (map toLower b)
    gettvStation onWhich programCol = onWhich programCol
