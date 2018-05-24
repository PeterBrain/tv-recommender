module Main where

--import Lib
--import Data.Char
import Text.HTML.TagSoup
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8


-- the channel number (ranging from 1 to length of the program list)
-- the broadcasting time
-- the station name
-- the title of the broadcast followed by its genre (if available)
data TvProgram = TvProgram {
  number :: String,
  time :: String,
  station :: String,
  title :: String,
  genre :: String,
  link :: String} deriving (Read, Eq)--Show,

instance Show TvProgram where
  show program = number program ++ ". " ++ time program ++ " " ++ station program ++ "\t" ++ title program ++ ", " ++ genre program ++ "\n"


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
  print $ map (\x -> L8.filter (/='\t') $ L8.filter (/='\n') $ fromTagText x) $ filter isTagText $ takeWhile (~/= TagOpen "" [("class","watchlist add")]) $ dropWhile (~/= TagOpen "" [("class","bc-content-container")]) content

  putStrLn "Got Content! Reading broadcast details ..."


listTvProgram :: IO ()
listTvProgram = do
  let manually1 = TvProgram {number ="001", time = "20:15-21:45", station = "ORF eins", title = "DOKeins: Bauer unser", genre = "Dokumentarfilm, \195\150/BEL/F 2016", link = ""}
  let manually2 = TvProgram {number ="002", time = "20:15-21:00", station = "ZDF infokanal", title = "Madame Mao - Aufstieg und Fall der Jiang Qing", genre = "Dokumentation, D 2017", link = ""}
  let manually3 = TvProgram {number ="003", time = "20:15-21:45", station = "ORF eins", title = "DOKeins: Bauer unser", genre = "Dokumentarfilm, \195\150/BEL/F 2016", link = ""}
  let manually4 = TvProgram {number ="004", time = "20:15-21:00", station = "ZDF infokanal", title = "Madame Mao - Aufstieg und Fall der Jiang Qing", genre = "Dokumentation, D 2017", link = ""}
  --let manually5 = [TvProgram {number="001", time= "20:15-21:45", station= "ORF eins", title= "DOKeins: Bauer unser", genre= "Dokumentarfilm, \195\150/BEL/F 2016", link= ""}, TvProgram {number="002", time= "20:15-21:00", station= "ZDF infokanal", title= "Madame Mao - Aufstieg und Fall der Jiang Qing", genre= "Dokumentation, D 2017", link= ""}, TvProgram {number="003", time= "20:15-21:45", station= "ORF eins", title= "DOKeins: Bauer unser", genre= "Dokumentarfilm, \195\150/BEL/F 2016", link= ""}, TvProgram {number="004", time= "20:15-21:00", station= "ZDF infokanal", title= "Madame Mao - Aufstieg und Fall der Jiang Qing", genre= "Dokumentation, D 2017", link= ""}]
  print manually1
  print manually2
  print manually3
  print manually4
  loop


loop :: IO ()
loop = do
  putStrLn "\nPlease enter a command or type 'help' for assistance!"
  command <- getLine
  case command of
    "list" -> listTvProgram
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
