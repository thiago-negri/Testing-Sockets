module Main where

import Network
import System.IO
import Control.Monad
import Control.Concurrent

type Message = String
type ChatUser = Int
data ChatMessage = ChatMessage { from :: ChatUser, text :: Message }

instance Show ChatMessage where
        show (ChatMessage user message) = show user ++ ": " ++ message 

port :: PortID
port = PortNumber 4422

main :: IO ()
main = withSocketsDo $ do
        userCount <- newMVar 0 -- Connections counter
        chan <- newChan -- Channel for broadcasting messages between Threads
        socket <- listenOn port -- Socket for server to listen
        handleConnections socket userCount chan -- Handle the connections

handleConnections :: Socket -> MVar ChatUser -> Chan ChatMessage -> IO ()
handleConnections socket userCount chan = do
        (socketHandle, _, _) <- accept socket
        thisUser <- newUser userCount
        forkIO $ handleUserConnection chan socketHandle thisUser
        handleConnections socket userCount chan
        
newUser :: MVar ChatUser -> IO ChatUser
newUser userCount = do
        currentValue <- readMVar userCount
        putMVar userCount (currentValue + 1)
        return (currentValue + 1)

handleUserConnection :: Chan ChatMessage -> Handle -> ChatUser -> IO ()
handleUserConnection chan socketHandle thisUser = do
        chanReader <- dupChan chan
        readerThread <- forkIO (startReader chanReader socketHandle thisUser)
        (startSender chan socketHandle thisUser)  -- TODO exceções... quit, etc
        killThread readerThread
        return ()

startReader :: Chan ChatMessage -> Handle -> ChatUser -> IO ()
startReader chanReader socketHandle thisUser = forever $ do
        message <- readChan chanReader
        when ((from message) /= thisUser) (display message socketHandle)
        
startSender :: Chan ChatMessage -> Handle -> ChatUser -> IO ()
startSender chan socketHandle thisUser = forever $ do
        line <- hGetLine socketHandle
        -- TODO verificar se a linha é uma mensagem ou um comando
        broadcast chan (ChatMessage thisUser line)
        return ()

broadcast :: Chan ChatMessage -> ChatMessage -> IO ()
broadcast chan message = do 
        writeChan chan message

display :: ChatMessage -> Handle -> IO ()
display message socketHandle = do
        hPutStrLn socketHandle (show message)