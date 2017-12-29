module Main where
import NotifyWindow
import NotifyServer
import Control.Monad (unless, forever)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  startServer drawMessage
  forever $ threadDelay 10000 
