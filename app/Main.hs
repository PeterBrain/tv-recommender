module Main where

--import Lib
--import Data.Char
import Text.HTML.TagSoup
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
	getTvProgramme
	loop

getTvProgramme :: IO ()
getTvProgramme = do
	putStrLn "Loading Programm ..."
	content <- parseTags <$> simpleHttp "https://www.tele.at/tv-programm/2015-im-tv.html?stationType=-1&start=0&limit=5&format=raw"
	--print content -- prints complete parsed content

	--print $ L8.filter (/='\t') $ L8.filter (/='\n') $ fromTagText $ head $ tail $ dropWhile (~/= TagOpen "" [("class","genre")]) content -- print all (everything)

	--print $ takeWhile (~/= TagOpen "" [("class","bc-item")]) $ dropWhile (~/= TagOpen "" [("class","bc-content-container")]) content -- print one film

	print $ L8.words $ innerText $ takeWhile (~/= TagOpen "" [("class","watchlist add")]) $ dropWhile (~/= TagOpen "" [("class","bc-content genre-marker-5 broadcast genreSelect genreType5")]) content -- print one film

	--print $ filter (~== TagOpen "" [("class","genre")]) content
	--let content_new = parseTags $ L8.filter (/='\t') $ L8.filter (/='\n') content
	--L8.putStrLn $ innerText $ dropWhile (~/= "<div class=station>") content_new
	--L8.putStrLn $ fromTagText $ dropWhile (~/= "<li>") content_new !! 1
	--let list = fromTagText $ dropWhile (~/= "<li>") content_new
	--L8.putStrLn $ fromTagText $ dropWhile (~/= "<div class=station>") content !! 1
	--print $ head $ L8.words $ innerText $ dropWhile (~/= "<div class=genre>") content

	--putStrLn "Got Content! Reading broadcast details ..."

--listTvProgramme = do

-- the channel number (ranging from 1 to length of the program list)
-- the broadcasting time
-- the station name
-- the title of the broadcast followed by its genre (if available)
--data TvProgramme = TvProgramme {number :: Int, time :: String, station :: String, title :: String, genre :: String, link :: String } deriving (Show, Read, Eq)
--myEntryAsString = "PhonebookEntry {firstname = \"John\", lastname = \"Doe\", address = \"34 Highstreet\", city = \"Georgetown\", county = \"Kensington\", postal = \"BH32 5TZ\", phone = \"01625-932209\", email = \"jd@test.com\"}"

loop :: IO ()
loop = do
	putStrLn "\nPlease enter a command or type 'help' for assistance!"
	command <- getLine
	case command of
		"exit" -> putStrLn "Bye!"
		"help" -> help
		_ -> do
			putStrLn $ "Command '" ++ command ++ "' is unknown!"
			loop

help :: IO ()
help = do
	putStrLn "\nThis Programm supports the following commands:"
	--putStrLn "\t'list' ... shows an overview of all broadcasts"
	--putStrLn "\t'show' n ... shows the details of the n-th entry in the programm list"
	--putStrLn "\t'add actor' name ... adds the given name to your list of favorite actors"
	--putStrLn "\t'list actors' ... shows a list of all your favorite actors"
	--putStrLn "\t'delete actor' name ... removes the given name from your list of favorite actors"
	--putStrLn "\t'recommend' ... shows a list of broadcast featuring your favorite actors"
	putStrLn "\t'exit' ... terminate the application"
	putStrLn "\t'help' ... shows this message"
	loop
