{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Telegram.Bot.API as Telegram
import qualified Telegram.Bot.Simple as Bot
import Telegram.Bot.Simple.UpdateParser (updateMessageText)
import Control.Monad.IO.Class (liftIO)
import Servant.Client (runClientM)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import System.Process
import System.Directory
import System.FilePath
import System.Exit
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception (try, SomeException)
import System.Environment (getEnv, lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig)
import GHC.Generics

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
        then Bot.replyText "⏳ Слишком много активных загрузок. Подожди немного..."
        else do
          Bot.replyText "🎧 Начинаю загрузку трека, подожди..."
          liftIO $ atomically $ modifyTVar' (activeDownloads model) (+1)
          let filename = "downloads/" ++ show (abs $ hash $ T.unpack url) ++ ".mp3"
          liftIO $ async $ do
            result <- try $ do
              createDirectoryIfMissing True "downloads"
              downloadAudio url filename
              exists <- doesFileExist filename
              when exists $ do
                size <- getFileSizeBytes filename
                when (size <= 50 * 1024 * 1024) $ do
                  sendAudioToChat (botToken model) chatId filename
                removeFile filename
            atomically $ modifyTVar' (activeDownloads model) (subtract 1)
            case result of
              Left (err :: SomeException) -> putStrLn $ "Error: " ++ show err
              Right _ -> return ()
          return ()
    else
      Bot.replyText "⚠️ Отправь мне ссылку с YouTube!\n\nПример:\nhttps://youtu.be/nOJSmXSFCWk?si=DOQmwjVjQnAIlzYS"
  pure NoOp

-- | Check if URL is from YouTube
isYouTubeUrl :: Text -> Bool
isYouTubeUrl url =
  "youtube.com" `T.isInfixOf` url ||
  "youtu.be" `T.isInfixOf` url

-- | Download audio using yt-dlp
downloadAudio :: Text -> FilePath -> IO ()
downloadAudio url output = do
  let cmd = unwords
        [ "yt-dlp -x --audio-format mp3 --audio-quality 0 --no-playlist -o"
        , output
        , T.unpack url
        ]
  (exitCode, _, stderr) <- readProcessWithExitCode "sh" ["-c", cmd] ""
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> error $ "yt-dlp failed: " ++ stderr

-- | Send audio file to chat
sendAudioToChat :: Telegram.Token -> Telegram.ChatId -> FilePath -> IO ()
sendAudioToChat token chatId filepath = do
  env <- Telegram.defaultTelegramClientEnv token
  let audio = Telegram.defSendAudio (Telegram.SomeChatId chatId) (Telegram.InputFile filepath "audio/mpeg")
  result <- runClientM (Telegram.sendAudio audio) env
  case result of
    Left err -> putStrLn $ "Failed to send audio: " ++ show err
    Right _ -> putStrLn "Audio sent successfully"
  return ()


-- | Get file size
getFileSizeBytes :: FilePath -> IO Integer
getFileSizeBytes = getFileSize

-- | Simple hash function
hash :: String -> Int
hash = foldl (\h c -> 33 * h + fromEnum c) 5381

-- | Main entry point
main :: IO ()
main = do
  putStrLn "🎵 Melodiλ - YouTube to Audio Telegram Bot"
  putStrLn "=========================================="
  
  -- Try to load .env file (ignore if doesn't exist)
  _ <- try (loadFile defaultConfig) :: IO (Either SomeException ())
  
  -- Get bot token from environment
  token <- getEnv "TELEGRAM_BOT_TOKEN"
  putStrLn "✓ Bot token loaded"
  
  -- Create downloads directory
  createDirectoryIfMissing True "downloads"
  putStrLn "✓ Downloads directory ready"
  
  -- Initialize STM counter
  downloadsVar <- newTVarIO 0
  
  -- Start bot
  putStrLn "✓ Starting bot..."
  let botToken = Telegram.Token $ T.pack token
  env <- Telegram.defaultTelegramClientEnv botToken
  Bot.startBot_ (bot downloadsVar botToken) env
