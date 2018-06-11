module Main where

--import Lib
import System.IO
import System.IO.Error -- error handling
import System.Directory -- rename/delte File

import Control.Exception -- error handling global
import Control.Exception.Base -- error handling global

import Text.Printf
import Text.HTML.TagSoup -- html/xml parsing

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8


handler_file_not_found :: IOError -> IO ()
handler_file_not_found e = do
    putStrLn "You do not have any favorite actors yet. Please use 'add actor' to add one.\n"


data TvProgramEntry = TvProgramEntry {
  number :: Int, -- the channel number (ranging from 1 to length of the program list)
  time :: String, -- the broadcasting time
  station :: String, -- the station name
  title :: String, -- the title of the broadcast followed by
  genre :: String, -- its genre (if available)
  link :: String} deriving (Read, Eq, Show)


printTvProgram :: TvProgramEntry -> IO ()
printTvProgram program = printf "%03i. %s %-15s %s, %s\n" (number program) (time program) (station program) (title program) (genre program)


main :: IO ()
main = do
  hSetEncoding stdin utf8
  tvListe <- getTvProgram
  loop tvListe


loop :: [TvProgramEntry] -> IO ()
loop tvList = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  command <- words <$> getLine

  let newCommand = if head command == "show" then head command else unwords $ take 2 command

  case newCommand of
    "list" -> do
      listTvProgram tvList
      loop tvList
    --"show" -> show (unwords $ tail command)
    "add actor" -> do
      actorAdd (unwords $ tail $ tail command)
      loop tvList
    "list actors" -> do
      actorList
      loop tvList
    "delete actor" ->do
      actorDel (unwords $ tail $ tail command)
      loop tvList
    --"recommend" -> do
    --  recommend
    --  loop tvList
    "exit" -> putStrLn "Bye!"
    "help" -> do
      help
      loop tvList
    _ -> do
      putStrLn $ "Command '" ++ unwords command ++ "' is unknown!"
      loop tvList


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


getTvProgram = do
  putStrLn "Loading Programm ..."
  content <- parseTags <$> simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=5&format=raw"

  let splittedHtml = map (map
       (L8.filter (/= '\t') . L8.filter (/= '\n') . fromTagText) . (filter isTagText .
       takeWhile (~/= TagOpen "" [("class", "watchlist add")]) .
       dropWhile (~/= TagOpen "" [("class", "bc-content-container")]))) $
       sections (~== "<div class=bc-item>") content

  let listOfPrograms = map (\x -> filter (/="") $ map L8.unpack x) splittedHtml

  let entries = map
       (\x -> if length x == 6 then
        TvProgramEntry 1 (x !! 1) (x !! 2) (x !! 3) (x !! 5) "" -- skip episode title
       else
        TvProgramEntry 1 (x !! 1) (x !! 2) (x !! 3) (x !! 4) "") listOfPrograms

  putStrLn "Got Content! Reading broadcast details ..."
  return entries


listTvProgram :: [TvProgramEntry] -> IO ()
listTvProgram list = do
  --mapM_ printTvProgram =<< getTvProgram
  --print =<< getTvProgram
  putStrLn ""
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
