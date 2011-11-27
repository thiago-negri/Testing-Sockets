module Main where

import Network
import System.IO
import Control.Monad
import Control.Concurrent
import Data.List (isPrefixOf)

type Message = String
data ChatUser = ChatUser { userId :: Int, username :: String }
        deriving Eq
data ChatMessage = ChatMessage { messageFrom :: ChatUser, messageText :: Message }
                 | QuitMessage { messageFrom :: ChatUser, messageText :: Message }
                 | JoinMessage { messageFrom :: ChatUser }

instance Show ChatUser where
        show (ChatUser userId username) = username ++ "(" ++ show userId ++ ")"

instance Show ChatMessage where
        show (ChatMessage user message) = show user ++ ": " ++ message 
        show (QuitMessage user message) = "! " ++ show user ++ " saiu: " ++ message
        show (JoinMessage user) = "! " ++ show user ++ " entrou."

port :: PortID
port = PortNumber 23

main :: IO ()
main = withSocketsDo $ do
        let userCount = 0 -- Connections counter
        chan <- newChan -- Channel for broadcasting messages between Threads
        socket <- listenOn port -- Socket for server to listen
        handleConnections socket userCount chan -- Handle the connections

handleConnections :: Socket -> Int -> Chan ChatMessage -> IO ()
handleConnections socket userCount chan = do
        (socketHandle, _, _) <- accept socket
        hSetBuffering socketHandle NoBuffering
        let nextUserNumber = (userCount + 1)
        forkIO $ handleUserConnection chan socketHandle nextUserNumber
        handleConnections socket nextUserNumber chan

handleUserConnection :: Chan ChatMessage -> Handle -> Int -> IO ()
handleUserConnection chan socketHandle thisUserNumber = do
        username <- readUsername socketHandle
	let thisUser = ChatUser thisUserNumber username
        broadcast chan $ JoinMessage thisUser
        chanReader <- dupChan chan
        readerThread <- forkIO (startReader chanReader socketHandle thisUser)
        quitMessage <- (startSender chan socketHandle thisUser) `catch` (\_ -> return "")
        broadcast chan $ QuitMessage thisUser quitMessage
        killThread readerThread
        hClose socketHandle

readUsername :: Handle -> IO String
readUsername socketHandle = do
        hPutStrLn socketHandle "Informe seu nome: "
        liftM (filter (/= '\r')) . readLine $ socketHandle -- remove o \r do final do nome do usuario

readLine :: Handle -> IO String
readLine = hGetLine

startReader :: Chan ChatMessage -> Handle -> ChatUser -> IO ()
startReader chanReader socketHandle thisUser = forever $ do
        message <- readChan chanReader
        when ((messageFrom message) /= thisUser) (display message socketHandle)
        
startSender :: Chan ChatMessage -> Handle -> ChatUser -> IO String
startSender chan socketHandle thisUser = do
        line <- readLine socketHandle
        if "/quit" `isPrefixOf` line
          then return $ drop 5 line
          else do
            broadcast chan (ChatMessage thisUser line)
            startSender chan socketHandle thisUser

broadcast :: Chan ChatMessage -> ChatMessage -> IO ()
broadcast chan message = do 
        writeChan chan message

display :: ChatMessage -> Handle -> IO ()
display message socketHandle = do
        hPutStrLn socketHandle (show message)