{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Telegram.Bot.API as Telegram
import qualified Telegram.Bot.Simple as Bot
import Telegram.Bot.Simple.UpdateParser (updateMessageText)
import Control.Monad.IO.Class (liftIO)
import Servant.Client (runClientM)
import Control.Monad (when, void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Process (readProcessWithExitCode, readCreateProcessWithExitCode, proc, StdStream(..), std_err)
import System.Directory
import System.IO (withFile, IOMode(..), hSetEncoding, hPutStrLn, utf8)
import System.Exit
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception (try, SomeException)
import System.Environment (getEnv)
import Configuration.Dotenv (loadFile, defaultConfig)
import GHC.Generics
import Data.Aeson (Value(..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | YouTube video metadata
data VideoMetadata = VideoMetadata
  { videoTitle :: Text
  , videoThumbnail :: Maybe Text
  , videoArtist :: Text
  } deriving (Show, Generic)

-- | Bot state model
data Model = Model
  { activeDownloads :: TVar Int
  , botToken :: Telegram.Token
  } deriving (Generic)

-- | Bot actions
data Action
  = NoOp
  | DownloadYouTube Telegram.ChatId Text
  deriving (Show)

-- | Initialize bot
bot :: TVar Int -> Telegram.Token -> Bot.BotApp Model Action
bot downloadsVar token = Bot.BotApp
  { Bot.botInitialModel = Model downloadsVar token
  , Bot.botAction = handleUpdate
  , Bot.botHandler = handleAction
  , Bot.botJobs = []
  }

-- | Parse incoming updates
handleUpdate :: Telegram.Update -> Model -> Maybe Action
handleUpdate update _ = do
  chatId <- Telegram.updateChatId update
  text <- updateMessageText update
  pure $ DownloadYouTube chatId text

-- | Handle bot actions
handleAction :: Action -> Model -> Bot.Eff Action Model
handleAction NoOp model = pure model

handleAction (DownloadYouTube chatId url) model = model Bot.<# do
  if isYouTubeUrl url
    then do
      count <- liftIO $ readTVarIO (activeDownloads model)
      if count >= 3
        then Bot.replyText "⏳ Too many active downloads. Please wait..."
        else do
          Bot.replyText "🎧 Starting download, please wait..."
          liftIO $ atomically $ modifyTVar' (activeDownloads model) (+1)
          let filename = "downloads/" ++ show (abs $ hash $ T.unpack url) ++ ".mp3"
          void $ liftIO $ async $ processDownload model chatId url filename
    else
      Bot.replyText "⚠️ Send me a YouTube link!\n\nExample:\nhttps://youtu.be/nOJSmXSFCWk?si=DOQmwjVjQnAIlzYS"
  pure NoOp

-- | Check if URL is from YouTube
isYouTubeUrl :: Text -> Bool
isYouTubeUrl url =
  "youtube.com" `T.isInfixOf` url ||
  "youtu.be" `T.isInfixOf` url

-- | Process download workflow
processDownload :: Model -> Telegram.ChatId -> Text -> FilePath -> IO ()
processDownload model chatId url filename = do
  result <- try $ do
    createDirectoryIfMissing True "downloads"
    metadata <- extractMetadata url
    downloadAudio url filename
    -- Clean metadata with ffmpeg to remove quotes from title
    cleanAudioMetadata filename (videoTitle metadata)
    whenFileExists filename $ \filePath -> do
      size <- getFileSize filePath
      when (size <= 50 * 1024 * 1024) $
        sendAudioWithMetadata (botToken model) chatId filePath metadata
      removeFile filePath
  atomically $ modifyTVar' (activeDownloads model) (subtract 1)
  either (\(err :: SomeException) -> putStrLn $ "Error: " ++ show err) (const $ return ()) result

-- | Helper to run action when file exists
whenFileExists :: FilePath -> (FilePath -> IO ()) -> IO ()
whenFileExists filePath action = do
  exists <- doesFileExist filePath
  when exists $ action filePath

-- | Extract YouTube video metadata
extractMetadata :: Text -> IO VideoMetadata
extractMetadata url = do
  let cmd = unwords
        [ "yt-dlp --dump-json --no-playlist"
        , T.unpack url
        ]
  (exitCode, stdout, _) <- readProcessWithExitCode "sh" ["-c", cmd] ""
  case exitCode of
    ExitSuccess -> parseMetadata (BL.fromStrict $ TE.encodeUtf8 $ T.pack stdout)
    ExitFailure _ -> return $ VideoMetadata "Unknown" Nothing "Unknown"

-- | Parse metadata from JSON
parseMetadata :: BL.ByteString -> IO VideoMetadata
parseMetadata jsonData = 
  case decode jsonData :: Maybe Value of
    Just (Object obj) -> return VideoMetadata
      { videoTitle = extractTitle obj
      , videoThumbnail = extractThumbnail obj
      , videoArtist = extractArtist obj
      }
    _ -> return $ VideoMetadata "Unknown" Nothing "Unknown"
  where
    extractTitle obj = case KM.lookup "title" obj of
      Just (String t) -> cleanTitle t
      _ -> "Unknown"
    
    -- Remove all types of quotes and extra whitespace from title
    cleanTitle t = T.strip $ T.filter (not . isQuote) t
      where
        isQuote c = c `elem` (['\"', '\'', '"', '"', '«', '»', '„', '"'] ++ ['\x2018', '\x2019'])
    
    extractThumbnail obj = case KM.lookup "thumbnail" obj of
      Just (String t) -> Just t
      _ -> Nothing
    
    extractArtist obj = case KM.lookup "channel" obj of
      Just (String a) -> a
      _ -> case KM.lookup "uploader" obj of
        Just (String u) -> u
        _ -> "Unknown"

-- | Download audio using yt-dlp with best quality
downloadAudio :: Text -> FilePath -> IO ()
downloadAudio url output = do
  let cmd = unwords
        [ "yt-dlp -x"
        , "--audio-format mp3"
        , "--audio-quality 0"      -- Best quality
        , "--embed-thumbnail"       -- Embed thumbnail in MP3
        , "--no-playlist"
        , "--concurrent-fragments 4"  -- Faster download
        , "-o", output
        , T.unpack url
        ]
  (exitCode, _, stderr) <- readProcessWithExitCode "sh" ["-c", cmd] ""
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure _ -> error $ "yt-dlp failed: " ++ stderr

-- | Clean audio metadata using ffmpeg - completely strip and rewrite ID3 tags
cleanAudioMetadata :: FilePath -> Text -> IO ()
cleanAudioMetadata filepath title = do
  let tempFile = filepath ++ ".tmp.mp3"  -- Use proper extension
  let metadataFile = filepath ++ ".metadata.txt"
  
  -- Write metadata to file with explicit UTF-8 encoding
  withFile metadataFile WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStrLn h ";FFMETADATA1"
    hPutStrLn h $ "title=" ++ T.unpack title
  
  -- Build ffmpeg command
  let args = 
        [ "-i", filepath
        , "-i", metadataFile  -- Read metadata from file
        , "-map", "0:a"  -- Copy audio stream from first input
        , "-map", "0:v?"  -- Copy video stream (thumbnail) if exists
        , "-map_metadata", "1"  -- Use metadata from second input (metadata file)
        , "-c", "copy"  -- Copy without re-encoding
        , "-id3v2_version", "3"  -- Use ID3v2.3
        , "-y", tempFile
        ]
  (exitCode, _, _) <- readCreateProcessWithExitCode ((proc "ffmpeg" args) { std_err = CreatePipe }) ""
  
  -- Cleanup metadata file
  removeFile metadataFile
  
  case exitCode of
    ExitSuccess -> do
      removeFile filepath
      renameFile tempFile filepath
    ExitFailure _ -> return ()  -- If ffmpeg fails, keep original file

-- | Send audio file with metadata to chat
sendAudioWithMetadata :: Telegram.Token -> Telegram.ChatId -> FilePath -> VideoMetadata -> IO ()
sendAudioWithMetadata token chatId filepath metadata = do
  clientEnv <- Telegram.defaultTelegramClientEnv token
  
  -- Download and send thumbnail if available
  thumbnailPath <- maybe (return Nothing) (downloadThumbnail filepath) (videoThumbnail metadata)
  
  let audio = configureAudio (Telegram.SomeChatId chatId) filepath metadata thumbnailPath
  result <- runClientM (Telegram.sendAudio audio) clientEnv
  
  -- Cleanup thumbnail
  maybe (return ()) removeFile thumbnailPath
  
  either 
    (\err -> putStrLn $ "Failed to send audio: " ++ show err)
    (const $ return ())
    result

-- | Configure audio message with metadata
configureAudio :: Telegram.SomeChatId -> FilePath -> VideoMetadata -> Maybe FilePath -> Telegram.SendAudioRequest
configureAudio chatId filepath metadata thumbnailPath =
  let baseAudio = Telegram.defSendAudio chatId (Telegram.InputFile filepath "audio/mpeg")
  in baseAudio
    { Telegram.sendAudioPerformer = Just (videoArtist metadata)
    , Telegram.sendAudioThumbnail = Telegram.InputFile <$> thumbnailPath <*> pure "image/jpeg"
    }  -- Don't set title - Telegram reads from file metadata

-- | Download thumbnail from URL
downloadThumbnail :: FilePath -> Text -> IO (Maybe FilePath)
downloadThumbnail audioPath url = do
  result <- try $ do
    manager <- newManager tlsManagerSettings
    request <- parseRequest $ T.unpack url
    response <- httpLbs request manager
    let thumbPath = audioPath ++ ".jpg"
    BL.writeFile thumbPath (responseBody response)
    return thumbPath
  return $ either (const Nothing :: SomeException -> Maybe FilePath) Just result


-- | Simple hash function
hash :: String -> Int
hash = foldl (\h c -> 33 * h + fromEnum c) 5381

-- | Main entry point
main :: IO ()
main = do
  -- Try to load .env file (ignore if doesn't exist)
  void $ (try (loadFile defaultConfig) :: IO (Either SomeException ()))
  
  -- Get bot token from environment
  token <- getEnv "TELEGRAM_BOT_TOKEN"
  
  -- Create downloads directory
  createDirectoryIfMissing True "downloads"
  
  -- Initialize STM counter and start bot
  downloadsVar <- newTVarIO 0
  let telegramToken = Telegram.Token $ T.pack token
  
  clientEnv <- Telegram.defaultTelegramClientEnv telegramToken
  Bot.startBot_ (bot downloadsVar telegramToken) clientEnv
