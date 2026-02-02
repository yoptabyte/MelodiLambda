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
import System.Process (readProcessWithExitCode, readCreateProcessWithExitCode, proc, StdStream(..), std_err, env)
import System.Directory
import System.Exit
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception (try, SomeException)
import System.Environment (getEnv, lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig)
import GHC.Generics
import Data.Aeson (Value(..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)
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
  -- Handle /download command or direct YouTube links
  let url = if "/download" `T.isPrefixOf` text
            then T.strip $ T.drop 9 text  -- Remove "/download " prefix
            else text
  pure $ DownloadYouTube chatId url

-- | Handle bot actions
handleAction :: Action -> Model -> Bot.Eff Action Model
handleAction NoOp model = pure model

handleAction (DownloadYouTube chatId url) model = model Bot.<# do
  -- Only respond to YouTube URLs (ignore other messages in groups)
  if isYouTubeUrl url
    then do
      count <- liftIO $ readTVarIO (activeDownloads model)
      if count >= 30
        then Bot.replyText "⏳ Too many active downloads. Please wait..."
        else do
          Bot.replyText "🎧 Starting download, please wait..."
          liftIO $ atomically $ modifyTVar' (activeDownloads model) (+1)
          let filename = "downloads/" ++ show (abs $ hash $ T.unpack url) ++ ".mp3"
          void $ liftIO $ async $ processDownload model chatId url filename
    else
      -- Don't reply to non-YouTube messages (to avoid spam in groups)
      return ()
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
  (exitCode, stdout, _) <- runYtDlpWithFallback
    [ "--dump-json"
    , "--no-playlist"
    , T.unpack url
    ]
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
  let args =
        [ "-x"
        , "--audio-format", "mp3"
        , "--audio-quality", "0"     -- Best quality
        , "--embed-thumbnail"
        , "--no-playlist"
        , "--concurrent-fragments", "32"
        , "--buffer-size", "64K"
        , "--http-chunk-size", "50M"
        , "--retries", "10"
        , "--fragment-retries", "10"
        , "--throttled-rate", "100K"
        , "-o", output
        , T.unpack url
        ]
  (exitCode, _, stderr) <- runYtDlpWithFallback args
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure _ -> error $ unlines $
      [ "yt-dlp failed:"
      , stderr
      , ""
      ]
      ++ formatYtDlpFailureHelp stderr

-- | Resolve yt-dlp executable path (overrideable via env).
getYtDlpExe :: IO FilePath
getYtDlpExe = fromMaybe "yt-dlp" <$> lookupEnv "MELODILAMBDA_YTDLP"

-- | Extra yt-dlp args, space-separated (no shell parsing).
getYtDlpExtraArgs :: IO [String]
getYtDlpExtraArgs = maybe [] words <$> lookupEnv "MELODILAMBDA_YTDLP_ARGS"

runYtDlp :: [String] -> IO (ExitCode, String, String)
runYtDlp args = do
  exe <- getYtDlpExe
  extra <- getYtDlpExtraArgs
  readProcessWithExitCode exe (extra ++ args) ""

shouldRetryWithFallback :: String -> Bool
shouldRetryWithFallback stderr =
  any (`isInfixOf` stderr)
    [ "HTTP Error 403"
    , "forcing SABR streaming"
    , "missing a url"
    ]

isDnsResolutionError :: String -> Bool
isDnsResolutionError stderr =
  any (`isInfixOf` stderr)
    [ "Failed to resolve"
    , "Name or service not known"
    , "Temporary failure in name resolution"
    , "Could not resolve"
    , "Could not contact DNS servers"
    ]

isYoutubeClientBreakage :: String -> Bool
isYoutubeClientBreakage stderr =
  any (`isInfixOf` stderr)
    [ "HTTP Error 403"
    , "forcing SABR streaming"
    , "missing a url"
    ]

-- | yt-dlp sometimes breaks for certain YouTube "player clients".
-- Try a small set of safer fallbacks only on known failure patterns.
runYtDlpWithFallback :: [String] -> IO (ExitCode, String, String)
runYtDlpWithFallback args = do
  (exitCode, stdout, stderr) <- runYtDlp args
  case exitCode of
    ExitSuccess -> pure (exitCode, stdout, stderr)
    ExitFailure _ ->
      if not (shouldRetryWithFallback stderr)
        then pure (exitCode, stdout, stderr)
        else do
          let fallbacks =
                [ ["--force-ipv4"]
                , ["--extractor-args", "youtube:player_client=android"]
                , ["--force-ipv4", "--extractor-args", "youtube:player_client=android"]
                , ["--extractor-args", "youtube:player_client=android,web"]
                ]
          tryFallbacks fallbacks (exitCode, stdout, stderr)
  where
    tryFallbacks [] lastResult = pure lastResult
    tryFallbacks (prefix:rest) _ = do
      result@(exitCode', _, _) <- runYtDlp (prefix ++ args)
      case exitCode' of
        ExitSuccess -> pure result
        ExitFailure _ -> tryFallbacks rest result

formatYtDlpFailureHelp :: String -> [String]
formatYtDlpFailureHelp stderr
  | isDnsResolutionError stderr =
      [ "Tip: this looks like a DNS/network issue (yt-dlp can't resolve youtube.com)."
      , "  - Check DNS in the same environment as the bot: `getent hosts www.youtube.com`."
      , "  - Check your resolver: `cat /etc/resolv.conf`."
      , "  - If you're on NixOS, ensure networking/DNS is configured (e.g. set `networking.nameservers`)."
      ]
  | isYoutubeClientBreakage stderr =
      [ "Tip: YouTube frequently breaks older yt-dlp builds."
      , "  - If you're using Nix, run `nix flake update` (or update your channels) and rebuild."
      , "  - Or point the bot at a newer yt-dlp via `MELODILAMBDA_YTDLP=/path/to/yt-dlp`."
      , "  - You can also pass extra flags via `MELODILAMBDA_YTDLP_ARGS` (e.g. `--extractor-args youtube:player_client=android`)."
      ]
  | otherwise =
      [ "Tip: check the URL, availability, and your network connection."
      ]

-- | Clean audio metadata using ffmpeg - completely strip and rewrite ID3 tags
cleanAudioMetadata :: FilePath -> Text -> IO ()
cleanAudioMetadata filepath title = do
  let tempFile = filepath ++ ".tmp.mp3"  -- Use proper extension

  -- Build ffmpeg command with UTF-8 title
  let titleStr = T.unpack title
  let args =
        [ "-i", filepath
        , "-map", "0:a"  -- Copy audio stream
        , "-map", "0:v?"  -- Copy video stream (thumbnail) if exists
        , "-c", "copy"  -- Copy without re-encoding
        , "-map_metadata", "-1"  -- Strip all metadata
        , "-id3v2_version", "3"  -- Use ID3v2.3
        , "-metadata", "title=" ++ titleStr
        , "-y", tempFile
        ]

  -- Set UTF-8 locale for the process
  let procConfig = (proc "ffmpeg" args)
        { std_err = CreatePipe
        , env = Just [("LANG", "en_US.UTF-8"), ("LC_ALL", "en_US.UTF-8")]
        }

  (exitCode, _, _) <- readCreateProcessWithExitCode procConfig ""

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
    { Telegram.sendAudioTitle = Just (videoTitle metadata)
    , Telegram.sendAudioPerformer = Just (videoArtist metadata)
    , Telegram.sendAudioThumbnail = Telegram.InputFile <$> thumbnailPath <*> pure "image/jpeg"
    }  -- Note: Telegram client adds quotes around title in UI - this is normal behavior

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
  void (try (loadFile defaultConfig) :: IO (Either SomeException ()))

  -- Get bot token from environment
  token <- getEnv "TELEGRAM_BOT_TOKEN"

  -- Create downloads directory
  createDirectoryIfMissing True "downloads"

  -- Initialize STM counter and start bot
  downloadsVar <- newTVarIO 0
  let telegramToken = Telegram.Token $ T.pack token

  clientEnv <- Telegram.defaultTelegramClientEnv telegramToken
  Bot.startBot_ (bot downloadsVar telegramToken) clientEnv
