module Main where

import Lib
--import Network.HTTP.Simple

main :: IO ()
main = do
	putStrLn "Loading Programm ..."

	--response <- simpleHttp "http://httpbin.org/get"
	--putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
	--print $ getResponseHeader "Content-Type" response
	--L8.putStrLn $ getResponseBody response

	putStrLn "Got Content! Reading broadcast details ..."
	loop

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
