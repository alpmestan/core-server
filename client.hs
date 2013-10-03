{-# LANGUAGE OverloadedStrings #-}

import Core.Simple
import Control.Concurrent
import Data.Aeson
import Network
import System.IO

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy            as T
import qualified Data.Text.Lazy.Encoding   as T
import qualified Data.Text.Lazy.IO         as T

queryCore :: T.Text -> IO (Maybe (Either GhcCoreError GhcCoreResult))
queryCore code = do
    handle <- connectTo "localhost" (PortNumber 9000)
    let str = encode (GhcArgs "7.6.3" O2 defaultCoreOutputOpts code)
    B.hPutStr handle str
    threadDelay 3000000
    resp <- readTilEmpty handle -- B.hGetNonBlocking handle (1024*500)
    hClose handle
    return $ decode resp
    
readTilEmpty :: Handle -> IO B.ByteString
readTilEmpty h = go 
    where go = do
              eof <- hIsEOF h
              if eof
                  then return B.empty
                  else do
                      b <- hReady h
                      if b
                          then do
                              s <- B.hGetNonBlocking h (1024*500)
                              rest <- go
                              return $ s `B.append` rest
                          else return B.empty

        