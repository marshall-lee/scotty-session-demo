{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T (concat, append, Text)
import qualified Data.Vault.Lazy as Vault
import Network.Wai (vault)
import Network.Wai.Session (withSession, SessionStore)
import Network.Wai.Session.Map (mapStore_)

type MySession = SessionStore IO () [T.Text]

main :: IO ()
main = do
  sessionStore <- mapStore_ :: IO MySession
  sessionKey <- Vault.newKey
  scotty 3000 $ do
    middleware $ withSession sessionStore "SESSION" def sessionKey
    get "/:word" $ do
      req <- request
      beam <- param "word"
      let Just (sessionLookup, sessionInsert) = Vault.lookup sessionKey (vault req)
       in do previousBeams <- liftIO $ fromMaybe [] <$> sessionLookup ()
             liftIO $ sessionInsert () (beam : previousBeams)
             html $ T.concat [
                 "<h1>Scotty, ", beam, " me up!</h1>",
                 "<p>Previous requests:<ul>",
                 T.concat $ fmap ("<li>" `T.append`) previousBeams,
                 "</ul></p>"
               ]
