{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent       (forkIO, threadDelay)
import Control.Concurrent.Async (async, wait, waitEither)
import Control.Monad            (forever)
import Core.Simple
import Data.Aeson
import Data.Hashable
import Data.Text                (Text)
import Data.Time                (getCurrentTime, UTCTime)
import GHC.Generics
import Network
import Options.Applicative
import System.IO

import qualified Data.ByteString.Lazy            as LB
-- import qualified Data.ByteString            as B
import qualified Data.Text.Lazy                  as T
import qualified Data.Text.Lazy.Encoding         as T

data Config = Config
  { cPort     :: Int    -- ^ port on which the server will listen 
  , cDelayGHC :: Int    -- ^ number of millisecs we give GHC
  , cHsDir    :: String -- ^ directory where we put the hs files
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
           <*> strOption
                   ( long "hsdir"
                  <> short 't'
                  <> metavar "HSDIR"
                  <> help "Store the haskell code in temporary files in the HSDIR directory")
                   
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
        forkIO (handleClient cfg handle) >> return ()
        
handleClient :: Config -> Handle -> IO ()
handleClient cfg handle = do
    request <- LB.hGetNonBlocking handle (1024*1000)
    maybe (LB.hPut handle . T.encodeUtf8 $ "Couldn't decode request: " `T.append` (T.decodeUtf8 request)) 
          (\ghcArgs -> do
              now <- getCurrentTime
              let modName = T.pack ("M" ++ (show . abs . hash . T.pack $ show now) )
              coreGenAsync <- async $ ghcCoreFor ghcArgs (cHsDir cfg) modName
              delayAsync   <- async $ threadDelay (cDelayGHC cfg * 1000)
              result       <- waitEither delayAsync coreGenAsync
              either (\_          -> LB.hPut handle . T.encodeUtf8 $ "Thread killed")
                     (\ghcCoreRes -> LB.hPut handle . encode $ ghcCoreRes)
                     result )
          (decode request)
    hClose handle

                      
main :: IO ()
main = execParser opts >>= server