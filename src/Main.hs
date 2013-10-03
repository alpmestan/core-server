{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent       (forkIO, threadDelay)
import Control.Concurrent.Async (async, waitEither)
import Control.Monad            (forever)
import Core.Simple
import Data.Aeson
import Data.Text.Lazy           (Text)
import Data.Time                (getCurrentTime, UTCTime)
import GHC.Generics
import Network
import Options.Applicative
import System.IO

import qualified Data.ByteString.Lazy    as LB
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.IO       as T
import qualified Data.Text.Lazy.Encoding as T

data Config = Config
  { cPort     :: Int -- ^ port on which the server will listen 
  , cDelayGHC :: Int -- ^ number of millisecs we give GHC
  }
 
configParser :: Parser Config
configParser = Config
           <$> option 
                   ( long "port"
                  <> short 'p'
                  <> metavar "PORT"
                  <> help "Make the server listen on port PORT")
           <*> option 
                   ( long "delay"
                  <> short 'd'
                  <> metavar "DELAY"
                  <> help "Give DELAY milliseconds to GHC before killing it")
                   
opts = info configParser
          ( fullDesc
         <> progDesc "A JSON server that generates GHC Core from Haskell code"
         <> header "core-server - generating GHC Core from Haskell code through JSON")

server :: Config -> IO ()
server cfg = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    listenSock <- listenOn . PortNumber . fromInteger . toInteger $ cPort cfg
    putStrLn $ "Server initialized, running on port " ++ show (cPort cfg)
    forever $ do
        (handle, _, _) <- accept listenSock
        putStrLn $ "New client"
        hSetBuffering handle NoBuffering
        forkIO (handleClient cfg handle) >> return ()
        
handleClient :: Config -> Handle -> IO ()
handleClient cfg handle = do
    request <- LB.hGetContents handle
    maybe (LB.hPutStr handle . T.encodeUtf8 $ "Couldn't decode request") 
          (\ghcArgs -> do
              LB.hPut stdout request
              coreGenAsync <- async $ ghcCoreFor ghcArgs
              delayAsync   <- async $ threadDelay (cDelayGHC cfg * 1000)
              result       <- waitEither delayAsync coreGenAsync
              either (\_          -> LB.hPut handle . T.encodeUtf8 $ "Thread killed")
                     (\ghcCoreRes -> LB.hPut handle . encode       $  ghcCoreRes)
                     result)
          (decode request)
    hClose handle

main :: IO ()
main = execParser opts >>= server