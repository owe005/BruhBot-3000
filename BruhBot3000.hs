{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (forM_, when)
import qualified Data.ByteString as B
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (transpose)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import Discord.Interactions
import qualified Discord.Requests as R
import Discord.Types
import UnliftIO (liftIO)
import UnliftIO.Concurrent

main :: IO ()
main =
  if testserverid == -1
    then TIO.putStrLn "ERROR: modify the source and set testserverid to your serverid"
    else interactionCommandExample

testserverid :: Snowflake
testserverid = 745725474465906732

void :: DiscordHandler (Either RestCallErrorCode b) -> DiscordHandler ()
void =
  ( >>=
      ( \case
          Left e -> liftIO $ print e
          Right _ -> return ()
      )
  )

interactionCommandExample :: IO ()
interactionCommandExample = do
  tok <- TIO.readFile "./examples/auth-token.secret"
  t <-
    runDiscord $
      def
        { discordToken = tok,
          discordOnStart = startHandler,
          discordOnEnd = liftIO $ putStrLn "Ended",
          discordOnEvent = eventHandler,
          discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }
  TIO.putStrLn t

startHandler :: DiscordHandler ()
startHandler = do
  -- Right partialGuilds <- restCall R.GetCurrentUserGuilds

  let activity =
        Activity
          { activityName = "Haskell Hell",
            activityType = ActivityTypeGame,
            activityUrl = Nothing
          }
  let opts =
        UpdateStatusOpts
          { updateStatusOptsSince = Nothing,
            updateStatusOptsGame = Just activity,
            updateStatusOptsNewStatus = UpdateStatusOnline,
            updateStatusOptsAFK = False
          }
  sendCommand (UpdateStatus opts)

  chans' <- restCall $ R.GetGuildChannels testserverid
  either
    (const (return ()))
    ( \chans ->
        forM_
          (take 1 (filter isTextChannel chans))
          ( \channel ->
              restCall $
                R.CreateMessage
                  (channelId channel)
                  "Hello!"
          )
    )
    chans'

-- | Example user command
exampleUserCommand :: Maybe CreateApplicationCommand
exampleUserCommand = createApplicationCommandUser "usercomm"

-- | An example slash command.
exampleSlashCommand :: Maybe CreateApplicationCommand
exampleSlashCommand =
  createApplicationCommandChatInput
    "test"
    "here is a description"
    >>= \cac ->
      return $
        cac
          { createApplicationCommandOptions =
              Just $
                ApplicationCommandOptionsValues
                  [ ApplicationCommandOptionValueString
                      "randominput"
                      "I shall not"
                      True
                      (Right [Choice "firstOpt" "yay", Choice "secondOpt" "nay"])
                  ]
          }

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (not (fromBot m) && isPing m) $ do
    -- A very simple message.
    void $ restCall (R.CreateMessage (messageChannelId m) "bruh")


isTextChannel :: Channel -> Bool
isTextChannel ChannelText {} = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("bruh" `T.isPrefixOf`) . T.toLower . messageContent
