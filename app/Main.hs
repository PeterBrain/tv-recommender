module Main where

--import Lib
import System.IO
import System.IO.Error -- error handling
import System.Directory -- rename/delte File

import Control.Exception -- error handling global
import Control.Exception.Base -- error handling global

import Text.HTML.TagSoup -- html/xml parsing
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8


handler_file_not_found :: IOError -> IO ()
handler_file_not_found e = do
    putStrLn "You do not have any favorite actors yet. Please use 'add actor' to add one.\n"


data TvProgramEntry = TvProgramEntry {
  number :: String, -- the channel number (ranging from 1 to length of the program list)
  time :: String, -- the broadcasting time
  station :: String, -- the station name
  title :: String, -- the title of the broadcast followed by
  genre :: String, -- its genre (if available)
  link :: String} deriving (Read, Eq, Show)


--instance Show TvProgramEntry where
--  show program = number program ++ ". " ++ time program ++ " " ++ station program ++ "\t" ++ title program ++ ", " ++ genre program ++ "\n"

printTvProgram :: TvProgramEntry -> IO ()
printTvProgram program = putStrLn $ number program ++ ". " ++ time program ++ " " ++ station program ++ "\t\t" ++ title program ++ ", " ++ genre program


--main :: IO ()
main = do
  TvListe <- getTvProgram
  loop TvListe


loop :: [TvProgramEntry] -> IO ()
loop TvList = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  command <- getLine
  let hcommand = drop 1 $ words command
  --if (head $ tail hcommand) == "actor" || (head $ tail hcommand) == "actors" then let command2 = head hcommand ++ " " ++ (head $ tail hcommand) else let command2 = head hcommand
  --command2 hcommand
  --  | (head $ tail hcommand) == "actor" =
  --  | (head $ tail hcommand) == "actors" =
  --if length hcommand == 1 then let command = command else let command = head hcommand
  case command of
    "list" -> do
      listTvProgram TvList
      loop TvList
    --"show" -> show
    --"add actor" -> actorAdd (tail $ tail hcommand)
    "list actors" -> do
      actorList
      loop TvList
    --"delete actor" -> actorDel (tail $ tail hcommand)
    --"recommend" -> recommend
    "exit" -> putStrLn "Bye!"
    "help" -> do
      help
      loop TvList
    _ -> do
      putStrLn $ "Command '" ++ command ++ "' is unknown!"
      loop TvList


help = do
  putStrLn "\nThis Programm supports the following commands:"
  putStrLn "\t'list' ... shows an overview of all broadcasts"
  --putStrLn "\t'show' n ... shows the details of the n-th entry in the programm list"
  putStrLn "\t'add actor' name ... adds the given name to your list of favorite actors"
  putStrLn "\t'list actors' ... shows a list of all your favorite actors"
  putStrLn "\t'delete actor' name ... removes the given name from your list of favorite actors"
  --putStrLn "\t'recommend' ... shows a list of broadcast featuring your favorite actors"
  putStrLn "\t'exit' ... terminate the application"
  putStrLn "\t'help' ... shows this message"


--getTvProgram :: IO ()
getTvProgram = do
  putStrLn "Loading Programm ..."
  content <- parseTags <$> simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=5&format=raw"
  --print content -- prints complete parsed content

  --print $ L8.filter (/='\t') $ L8.filter (/='\n') $ fromTagText $ head $ tail $ dropWhile (~/= TagOpen "" [("class","genre")]) content -- print all (everything)

  --print $ takeWhile (~/= TagOpen "" [("class","bc-item")]) $ dropWhile (~/= TagOpen "" [("class","bc-content-container")]) content -- print one film

  --print $ L8.words $ innerText $ takeWhile (~/= TagOpen "" [("class","watchlist add")]) $ dropWhile (~/= TagOpen "" [("class","bc-content genre-marker-5 broadcast genreSelect genreType5")]) content -- print one film

  --parseTvProgram content
  --  | [] resultList = return 1
  --  | (h:t) resultList = parseFunction h : parseTvProgram t

  --let parsedHtml = fmap (\x -> L8.filter (/='\t') $ L8.filter (/='\n') $ fromTagText x) $ filter isTagText $ takeWhile (~/= TagOpen "" [("class","watchlist add")]) $ dropWhile (~/= TagOpen "" [("class","bc-content-container")]) content
  --print $ filter (/="") $ map (L8.unpack) parsedHtml

  let splittedHtml = map (map (\x -> L8.filter (/='\t') $ L8.filter (/='\n') $ fromTagText x)) $ map (\x -> filter isTagText $ filter (~/= TagOpen "" [("class","episode")]) $ takeWhile (~/= TagOpen "" [("class","watchlist add")]) $ dropWhile (~/= TagOpen "" [("class","bc-content-container")]) x) $ sections (~== "<div class=bc-item>") content

  let listofprogramms = map (\x -> filter (/="") $ map L8.unpack x) splittedHtml

  let entries = map (\x -> if length x == 6 then TvProgramEntry "001" (x !! 1) (x !! 2) (x !! 3) (x !! 5) "" else TvProgramEntry "001" (x !! 1) (x !! 2) (x !! 3) (x !! 4) "") listofprogramms
  --print entries
  --mapM_ printTvProgram entries
  putStrLn "Got Content! Reading broadcast details ..."
  return entries


listTvProgram :: [TvProgramEntry] -> IO ()
listTvProgram list = do
  --mapM_ printTvProgram =<< getTvProgram
  --print =<< getTvProgram
  --liste <- getTvProgram
  mapM_ printTvProgram list




find' :: (a->Bool) -> [a] -> Bool
find' _ [] = False
find' func (h:t)
  | func h = True
  | otherwise = find' func t


actorAdd :: String -> IO ()
actorAdd name = do
  let
      outFile = "actors.txt"
      tempFile = "actors.tmp"

  fileExist <- doesFileExist outFile
  if not fileExist then writeFile outFile "" else return ()

  handle <- openFile outFile ReadMode
  fileContent <- hGetContents handle

  let
    append
      | not checkDouble = fileContent ++ name ++ "\n"
      | otherwise = fileContent
    checkDouble = find' (==name) (lines fileContent)

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

  let filterFile = unlines $ filter (/=name) (lines fileContent)
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
  putStrLn $ "\n" ++ fileContent
  hClose handle

  `catch` handler_file_not_found
