import Network.Socket
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
 
hGetLineStripped hdl = liftM (filter (not . flip elem "\n\r")) (hGetLine hdl)

main = do
  index <- newTVarIO []
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 5999 iNADDR_ANY)
  listen sock 2
  mainLoop index sock 0

mainLoop index sock n = do
  conn <- accept sock
  forkIO (runConn index conn ("user" ++ show n))
  mainLoop index sock $! n+1
 
runConn index (sock, _) name = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  handle (\(SomeException _) -> return ()) $ (do arrive index name hdl
                                                 connLoop index name hdl)
  depart index name hdl
  hClose hdl

broadcast str conns = sequence_ $ map sendStr conns
    where sendStr (_, hdl) = hPutStrLn hdl str
 
arrive index name hdl = do
  hPutStrLn hdl ("you are " ++ name)
  old <- atomically $ do old <- readTVar index
                         writeTVar index ((name, hdl) : old)
                         return old
  broadcast (name ++ " arrived") old
  sequence_ $ map (\ (otherName, _) -> hPutStrLn hdl (otherName ++ " arrived")) old

depart index name hdl = do
  new <- atomically $ do old <- readTVar index
                         let new = old \\ [(name, hdl)]
                         writeTVar index new
                         return new
  broadcast (name ++ " departed") new

connLoop index name hdl = do
  line <- hGetLineStripped hdl
  curr <- atomically $ readTVar index
  broadcast (name ++ " says " ++ line) curr
  connLoop index name hdl
