{-# LANGUAGE OverloadedStrings #-}  
import Control.Monad
import Data.Text as T
import Data.List as L
import qualified Data.Text.IO as TIO
import UnliftIO.Concurrent
import Discord
import Discord.Types
import qualified Discord.Requests as R

main :: IO ()
main = do 
    userFacingError <- runDiscord $ def
             { discordToken = "Bot TOKEN"
             , discordOnEvent = eventHandler
             , discordOnLog = \s -> TIO.putStrLn s
             , discordForkThreadForEvents = True }
    TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
       MessageCreate m -> when (not (fromBot m) && checkForBruh m) $ do
               void $ restCall (R.CreateMessage (messageChannelId m) "bruh")

       _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

checkForBruh :: Message -> Bool
checkForBruh = ("bruh" `T.isInfixOf`) . toLower . messageContent

{- Unused functionality

bruhCheck :: Eq a => [a] -> [a] -> Bool
bruhCheck a b = a `L.isInfixOf` b

-} 