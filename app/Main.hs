{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8           as BC (unpack)
import qualified Data.ByteString.Lazy            as LB (empty, readFile)
import qualified Data.Text                       as T (pack, unpack)
import           Network.Mime                    (MimeType, defaultMimeLookup)
import           System.Directory                (doesFileExist)
import           System.FilePath                 (dropDrive, (</>))
-- import           VipsImage
import           HIP

import           Data.Streaming.Network.Internal (HostPreference (Host))
import qualified Data.Text.Lazy                  as LT (pack)
import           Network.HTTP.Types              (status404, status500)
import           Network.Wai                     (Request (..))
import           Network.Wai.Handler.Warp        (setHost, setPort)
import           Web.Scotty                      (ActionM, RoutePattern,
                                                  ScottyM, function, get, param,
                                                  raw, rescue, scottyOpts,
                                                  setHeader, settings, status,
                                                  text)

import           Data.Default.Class              (def)

import           Control.Monad.IO.Class          (liftIO)

import           Data.Semigroup                  ((<>))
import           Options.Applicative

data Options = Options { getHost :: String
                       , getPort :: Int
                       , getPath :: FilePath
                       }

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

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "VipsImage"
     <> header "image-vips - VipsImage server" )

program :: Options -> IO ()
program Options{getHost = host, getPort = port, getPath = path} =
  scottyOpts opts $ application path

  where opts = def { settings = setPort port $ setHost (Host host) (settings def) }

application :: FilePath -> ScottyM ()
application root = do
  get matchPath $ getFileHandler root

matchPath :: RoutePattern
matchPath = function $ \req ->
  Just [("path", LT.pack $ foldr ((</>) . T.unpack) "" (pathInfo req))]

getFileHandler :: FilePath -> ActionM ()
getFileHandler root = do
  path <- filePath root
  width <- fromIntegral <$> param "width" `rescue` (\_ -> return (0 :: Int))
  fileExists <- liftIO $ doesFileExist path
  if fileExists then sendFileHandler path width
                else status status404 >> raw LB.empty

sendFileHandler :: FilePath -> Int -> ActionM ()
sendFileHandler path 0 = do
  setHeader "Content-Type" $ LT.pack $ BC.unpack $ getMimeType path
  raw =<< liftIO (LB.readFile path)

sendFileHandler path w = do
  setHeader "Content-Type" "image/jpeg"
  buf <- liftIO $ flip resizeImage w =<< LB.readFile path
  case buf of
    Left e    -> status status500 >> text (LT.pack e)
    Right out -> raw out

filePath :: FilePath -> ActionM FilePath
filePath root = do
  path <- dropDrive <$> param "path"
  return $ root </> path

-- | Guess MIME type from file extension
getMimeType :: FilePath -> MimeType
getMimeType = defaultMimeLookup . T.pack
