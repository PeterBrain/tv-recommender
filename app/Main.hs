module Main where

--import Lib
--import Data.Char
import Text.HTML.TagSoup
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8


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
printTvProgram program = putStrLn $ number program ++ ". " ++ time program ++ " " ++ station program ++ "\t" ++ title program ++ ", " ++ genre program

type TvProgram = [TvProgramEntry]

main :: IO ()
main = do
  getTvProgram
  loop


getTvProgram :: IO ()
getTvProgram = do
  putStrLn "Loading Programm ..."
  content <- parseTags <$> simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=5&format=raw"
  --print content -- prints complete parsed content

  --L8.putStrLn $ innerText $ dropWhile (~/= "<div class=station>") content
  --L8.putStrLn $ fromTagText $ dropWhile (~/= "<div class=station>") content !! 1
  --print $ head $ L8.words $ innerText $ dropWhile (~/= "<div class=genre>") content

  --print $ L8.filter (/='\t') $ L8.filter (/='\n') $ fromTagText $ head $ tail $ dropWhile (~/= TagOpen "" [("class","genre")]) content -- print all (everything)

  --print $ takeWhile (~/= TagOpen "" [("class","bc-item")]) $ dropWhile (~/= TagOpen "" [("class","bc-content-container")]) content -- print one film

  --print $ L8.words $ innerText $ takeWhile (~/= TagOpen "" [("class","watchlist add")]) $ dropWhile (~/= TagOpen "" [("class","bc-content genre-marker-5 broadcast genreSelect genreType5")]) content -- print one film

  --parseTvProgram content
  --  | [] resultList = return 1
  --  | (h:t) resultList = parseFunction h : parseTvProgram t

  --let parsedHtml = fmap (\x -> L8.filter (/='\t') $ L8.filter (/='\n') $ fromTagText x) $ filter isTagText $ takeWhile (~/= TagOpen "" [("class","watchlist add")]) $ dropWhile (~/= TagOpen "" [("class","bc-content-container")]) content
  --print $ filter (/="") $ map (L8.unpack) parsedHtml

  --let listofprogramms = filter (/="") $ map L8.unpack parsedHtml
  --let myentry = TvProgramEntry "000" (head $ tail listofprogramms) (head $ tail $ tail listofprogramms) (head $ tail $ tail $ tail listofprogramms) (head $ tail $ tail $ tail $ tail listofprogramms) ""
  --print myentry

  let splittedHtml = map (map (\x -> L8.filter (/='\t') $ L8.filter (/='\n') $ fromTagText x)) $ map (\x -> filter isTagText $ takeWhile (~/= TagOpen "" [("class","watchlist add")]) $ dropWhile (~/= TagOpen "" [("class","bc-content-container")]) x) $ sections (~== "<div class=bc-item>") content

  let listofprogramms2 = map (\x -> filter (/="") $ map L8.unpack x) splittedHtml

  --let mynewentry = map (\x -> TvProgramEntry "000" (head $ tail x) (head $ tail $ tail x) (head $ tail $ tail $ tail x) (head $ tail $ tail $ tail $ tail x) "") listofprogramms2

  let mynewentry = map (\x -> TvProgramEntry "000" (x !! 1) (x !! 2) (x !! 3) (x !! 4) "") listofprogramms2
  mapM_ printTvProgram mynewentry
  --print mynewentry

  putStrLn "Got Content! Reading broadcast details ..."


listTvProgram :: IO ()
listTvProgram = do
  --let manually = [TvProgramEntry {number="001", time= "20:15-21:45", station= "ORF eins", title= "DOKeins: Bauer unser", genre= "Dokumentarfilm, \195\150/BEL/F 2016", link= ""}, TvProgramEntry {number="002", time= "20:15-21:00", station= "ZDF infokanal", title= "Madame Mao - Aufstieg und Fall der Jiang Qing", genre= "Dokumentation, D 2017", link= ""}, TvProgramEntry {number="003", time= "20:15-21:45", station= "ORF eins", title= "DOKeins: Bauer unser", genre= "Dokumentarfilm, \195\150/BEL/F 2016", link= ""}, TvProgramEntry {number="004", time= "20:15-21:00", station= "ZDF infokanal", title= "Madame Mao - Aufstieg und Fall der Jiang Qing", genre= "Dokumentation, D 2017", link= ""}]
  --let manually2 = TvProgramEntry "001" "20:15-21:45" "ORF eins" "DOKeins: Bauer unser" "Dokumentarfilm, \195\150/BEL/F 2016" ""
  --print manually
  --print manually2

  loop


loop :: IO ()
loop = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  command <- getLine
  case command of
    "list" -> listTvProgram
    --"show" -> show
    --"add actor" -> actorAdd
    --"list actors" -> actorsList
    --"delete actor" -> actorDel
    --"recommend" -> recommend
    "exit" -> putStrLn "Bye!"
    "help" -> help
    _ -> do
      putStrLn $ "Command '" ++ command ++ "' is unknown!"
      loop


help :: IO ()
help = do
  putStrLn "\nThis Programm supports the following commands:"
  putStrLn "\t'list' ... shows an overview of all broadcasts"
  --putStrLn "\t'show' n ... shows the details of the n-th entry in the programm list"
  --putStrLn "\t'add actor' name ... adds the given name to your list of favorite actors"
  --putStrLn "\t'list actors' ... shows a list of all your favorite actors"
  --putStrLn "\t'delete actor' name ... removes the given name from your list of favorite actors"
  --putStrLn "\t'recommend' ... shows a list of broadcast featuring your favorite actors"
  putStrLn "\t'exit' ... terminate the application"
  putStrLn "\t'help' ... shows this message"
  loop
