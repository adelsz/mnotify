{-# LANGUAGE OverloadedStrings #-}

module NotifyServer (
  startServer
  ) where

import DBus
import DBus.Client
import DBus.Internal.Types
import Data.List (sort)
import Data.Maybe
import Debug.Trace
import Data.Map (Map)
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)
import Data.Int

getServerInformation :: IO (String, String, String, String)
getServerInformation = return ("mnotify", "mnotify","0.1","0.1")

getCapabilities :: IO [String]
getCapabilities = return (traceShowId ["body"])

notify :: (String -> String -> IO ()) -> MethodCall -> IO Reply
notify drawRoutine mCall = (drawRoutine vSummary vBody) >> return reply
      where reply = replyReturn [toVariant (0::Int32)]
            bodyVariants = methodCallBody mCall
            [name, rid, icon, summary, body, actions, hints, expire] = bodyVariants
            vBody = unVar "No body found" body
            vSummary = unVar "No summary" summary

unVar :: (IsVariant a) => a -> Variant -> a
unVar defaultValue variant = fromMaybe defaultValue (fromVariant variant)

notifyInSignature = [
    TypeString,
    TypeInt32,
    TypeString,
    TypeString,
    TypeString,
    TypeArray TypeString,
    TypeDictionary TypeString TypeString,
    TypeInt32
  ]

exportAndWait :: Client -> RequestNameReply -> (String -> String -> IO()) -> IO () 
exportAndWait client NameExists _ = do
  putStrLn "Nmonad is already running. Exiting..."
exportAndWait client _ showMsgCallback = do 
  export client "/org/freedesktop/Notifications" [ 
      autoMethod "org.freedesktop.Notifications" "GetServerInformation" getServerInformation,  
      autoMethod "org.freedesktop.Notifications" "GetCapabilities" getCapabilities,
      method "org.freedesktop.Notifications"  "Notify" (signature_ notifyInSignature) (signature_ [TypeInt32]) (notify showMsgCallback)
    ]

startServer :: (String -> String -> IO()) -> IO ()
startServer showMsgCallback = do
 client <- connectSession
 requestReply <- requestName client (busName_ "org.freedesktop.Notifications") [nameDoNotQueue]
 exportAndWait client requestReply showMsgCallback
