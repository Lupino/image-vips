{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8           as BC (unpack)
import qualified Data.ByteString.Lazy            as LB (ByteString, empty,
                                                        readFile)
import           Data.LruCache.IO                (LruHandle, cached,
                                                  newLruHandle)
import qualified Data.Text                       as T (pack, unpack)
import           HIP
import           Network.Mime                    (MimeType, defaultMimeLookup)
import           System.Directory                (doesFileExist)
import           System.FilePath                 (dropDrive, (</>))

import           Data.Streaming.Network.Internal (HostPreference (Host))
import qualified Data.Text.Lazy                  as LT (pack)
import           Network.HTTP.Types              (status404)
import           Network.Wai                     (Request (..))
import           Network.Wai.Handler.Warp        (setHost, setPort)
import           Web.Scotty                      (ActionM, RoutePattern,
                                                  ScottyM, function, get, param,
                                                  raw, rescue, scottyOpts,
                                                  setHeader, settings, status)

import           Data.Default.Class              (def)

import           Control.Monad.IO.Class          (liftIO)

import           Data.Semigroup                  ((<>))
import           Options.Applicative

data Options = Options { getHost    :: String
                       , getPort    :: Int
                       , getPath    :: FilePath
                       , getLruSize :: Int
                       }

type LruCache = LruHandle String LB.ByteString

parser :: Parser Options
parser = Options <$> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> value "127.0.0.1")

                 <*> option auto (long "port"
                                  <> short 'p'
                                  <> metavar "PORT"
                                  <> value 3000)

                 <*> strOption (long "path"
                                <> metavar "PATH"
                                <> value ".")
                 <*> option auto (long "size"
                                  <> metavar "LRUSIZE"
                                  <> value 1000)

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "VipsImage"
     <> header "image-vips - VipsImage server" )

program :: Options -> IO ()
program Options{getHost = host, getPort = port, getPath = path, getLruSize = size} = do
  lru <- newLruHandle size
  scottyOpts opts $ application path lru

  where opts = def { settings = setPort port $ setHost (Host host) (settings def) }

application :: FilePath -> LruCache -> ScottyM ()
application root lru = get matchPath $ getFileHandler root lru

matchPath :: RoutePattern
matchPath = function $ \req ->
  Just [("path", LT.pack $ foldr ((</>) . T.unpack) "" (pathInfo req))]

getFileHandler :: FilePath -> LruCache -> ActionM ()
getFileHandler root lru = do
  path <- filePath root
  width <- param "width" `rescue` (\_ -> return (0 :: Int))
  fileExists <- liftIO $ doesFileExist path
  if fileExists then sendFileHandler path width lru
                else status status404 >> raw LB.empty

sendFileHandler :: FilePath -> Int -> LruCache -> ActionM ()
sendFileHandler path 0 _ = do
  setHeader "Content-Type" $ LT.pack $ BC.unpack $ getMimeType path
  raw =<< liftIO (LB.readFile path)

sendFileHandler path w lru = do
  setHeader "Content-Type" "image/jpeg"
  buf <- liftIO $ cached lru ( path ++ show w ) $ do
    r <- flip resizeImage w =<< LB.readFile path
    case r of
      Left _    -> pure LB.empty
      Right out -> pure out

  raw buf

filePath :: FilePath -> ActionM FilePath
filePath root = do
  path <- dropDrive <$> param "path"
  return $ root </> path

-- | Guess MIME type from file extension
getMimeType :: FilePath -> MimeType
getMimeType = defaultMimeLookup . T.pack
