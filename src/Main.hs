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
import System.IO

import qualified Data.ByteString.Lazy    as LB
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.IO       as T
import qualified Data.Text.Lazy.Encoding as T

data Config = Config
  { cPort     :: PortNumber -- ^ port on which the server will listen 
  , cDelayGHC :: Int        -- ^ number of millisecs we give GHC
  }

server :: Config -> IO ()
server cfg = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    listenSock <- listenOn $ PortNumber (cPort cfg)
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
main = let cfg = Config 9000 (3*1000) in server cfg