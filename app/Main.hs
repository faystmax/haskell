module Main where

import Control.Concurrent
import Control.Monad (forever)
import Control.Concurrent.Chan

pinger chanPingPong chanPongPing = do
    putStrLn "Ping: Pinging"
    writeChan chanPingPong "ping"
    putStrLn "Ping: Waiting for a pong"
    reply <- readChan chanPongPing
    putStrLn $ "Ping: " ++ reply ++ " received, done"

ponger chanPingPong chanPongPing = do
    putStrLn "Pong: Waiting for a ping"
    getPing <- readChan chanPingPong
    putStrLn $ "Pong: " ++ getPing ++ " received, sending pong"
    writeChan chanPongPing "pong"
    putStrLn "Pong: done"

forkCreater action = do
    forkIO $ forever action


main :: IO()
main = do
    putStrLn "START!"
    -- создание каналов
    chanPingPong <- newChan
    chanPongPing <- newChan
    -- создание процессов и передача им каналов
    forkCreater $ pinger chanPingPong chanPongPing
    forkCreater $ ponger chanPingPong chanPongPing
    -- ожидание завершения (нажатия enter)
    getLine
    return ()


