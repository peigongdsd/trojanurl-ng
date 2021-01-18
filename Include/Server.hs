{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Include.Server(
    runserver
) where

import Include.Persistent
    ( ProxyConfig(name),
      Persistent(Persistent),
      PerConf,
      rollback,
      update,
      InternalError(Error),
      get )
import Include.Fetch
    ( getSubsLinks, splitSubsLinks, readSubsLink, genConfigJson )

import Control.Exception ( throw, catch, evaluate )
import Data.IORef ( modifyIORef, newIORef, readIORef )
import Data.List ( sortBy )

import Control.Concurrent
import Control.Monad
import System.Cron
import Data.Time.Clock
import Network.Wai
    ( 
    responseLBS,
    Application,
    Request(rawPathInfo),
    ResponseReceived )
import Network.Wai.Handler.Warp
    ( setHost, setPort, runSettings, defaultSettings, Port, HostPreference )
import Network.HTTP.Types ( status200, status404 )
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BZ
import Data.List ( sort )
import Data.String ( IsString(fromString) )
import GHC.Generics ( Generic )
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
    ( ToJSON(toJSON), encode, object, Value(String), KeyValue((.=)) )
import Text.Regex.PCRE ( (=~) )

port :: Port
port = 2021

subscribeApp :: PerConf -> Application 
subscribeApp config req respond  
    | path =~ ("^/lsnode/?$" :: String) = respond200 =<< srvListNode config
    | path =~ ("^/packup/?$" :: String) = respond200 =<< return "Method not implemented yet!"
    | path =~ ("^/refresh/?$" :: String) = respond200 =<< srvRefresh config
    | path =~ ("^/fallback/?$" :: String) = respond200 =<< srvFallback config
    | path =~ ("^/download/[0-9]*/?$" :: String) = respond200 =<< srvDownload config path
    | otherwise = respond $ responseLBS status404 [] ""
    where
        path = rawPathInfo req
        respond200 :: B.ByteString -> IO ResponseReceived
        respond200 = respond . responseLBS status200 [] . BZ.fromStrict 

srvRefresh :: PerConf -> IO B.ByteString 
srvRefresh config = catch (do
    links <- sortBy (\p q -> compare (name p) (name q)) . 
        fmap readSubsLink . splitSubsLinks <$> getSubsLinks >>= evaluate
    modifyIORef config $ update links
    return "Successfully refreshed proxy profile!")
        (\(Error s) -> return . fromString $ s)

srvFallback :: PerConf -> IO B.ByteString 
srvFallback config = catch
    (modifyIORef config rollback >> return "Successfully roll back proxy profile!")
    (\(Error s) -> return . fromString $ s)

srvListNode :: PerConf -> IO B.ByteString 
srvListNode config = catch (do
    links <- transLinks 0 . get <$> readIORef config >>= evaluate
    return . BZ.toStrict . encode $ object ["status" .= True, "data" .= object links]
    ) (\(Error s) -> return . fromString $ s) where
        transLinks :: Int -> [ProxyConfig] -> [(T.Text, Value)]
        transLinks _ [] = []
        transLinks i (p : ps) = (T.pack . show $ i, String . T.pack . B8.unpack . name $ p) : transLinks (i + 1) ps

srvDownload :: PerConf -> B.ByteString -> IO B.ByteString 
srvDownload config label = catch (do
    links <- get <$> readIORef config >>= evaluate
    if index >= length links then throw . Error $ "Invalid index!!" else
        return . genConfigJson . (!! index) $ links) (\(Error s) -> return . fromString $ s) where
            index = (read . B8.unpack :: B.ByteString -> Int) $ label =~ ("(?<=^/download/)[0-9]*(?=/?$)" :: String)

scheduleRefresh :: PerConf -> IO ()
scheduleRefresh config = forever $ catch (do
    now <- getCurrentTime
    when (scheduleMatches daily now) . void $ srvRefresh config
    putStrLn "sleeping"
    threadDelay 100000) (\(Error s) -> putStrLn $ "Error in scheduly subscribing due to " ++ s)

runserver :: HostPreference -> Port -> IO ()
runserver h p = do
    confData <- newIORef $ Persistent Nothing Nothing
    forkIO $ scheduleRefresh confData
    runSettings (setHost h . setPort p $ defaultSettings) $ subscribeApp confData
