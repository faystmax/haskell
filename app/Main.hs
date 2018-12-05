module Main where

import Control.Concurrent
import Control.Monad (forever)
import Control.Concurrent.Chan
import Control.Concurrent

data StreetState = StreetState { leaves :: Bool, fantic :: Bool} deriving Show

wind :: Chan [Char] -> IO()
wind toTree = do
    putStrLn "Wind: send Wind"
    writeChan toTree "wind"
    threadDelay 5000000

tree :: Chan [Char] -> Chan [Char] -> IO()
tree toTree toStreet = do
    putStrLn "Tree: Waiting for something"
    something <- readChan toTree
    putStrLn $ "Tree: received = " ++ something
    writeChan toStreet "leaves"

street :: Chan [Char] -> Chan [Char] -> StreetState -> IO()
street toStreet toJanitor state = do
    print $ "Street: Waiting for something. State = " ++ show state
    received <- readChan toStreet
    putStrLn $ "Street: received = " ++ received
    parseActionAnswer received toJanitor state
    street toStreet toJanitor $ parseStateAnswer received state

parseStateAnswer :: [Char] -> StreetState -> StreetState
parseStateAnswer "leaves" state = state {leaves = True}
parseStateAnswer "clean" state = state {leaves = False, fantic = False}
parseStateAnswer answer state = state

parseActionAnswer :: [Char] -> Chan [Char] -> StreetState -> IO()
parseActionAnswer "isClean" toJanitor StreetState{leaves = False , fantic = False} =  writeChan toJanitor "yes"
parseActionAnswer "isClean" toJanitor state =  writeChan toJanitor "no"
parseActionAnswer answer toJanitor state =  return ()


janitor :: Chan [Char] -> Chan [Char] -> IO()
janitor toStreet toJanitor = do
    print $ "Janitor: Sending isClean to Street "
    writeChan toStreet "isClean"
    received <- readChan toJanitor
    putStrLn $ "Janitor: received = " ++ received
    parseJanitorAnswer received toStreet
    threadDelay 7000000

parseJanitorAnswer :: [Char] -> Chan [Char] -> IO()
parseJanitorAnswer "no"  toStreet =  writeChan toStreet "clean"
parseJanitorAnswer "yes" toStreet =  return ()

forkCreator action = do
    forkIO $ forever action

main :: IO()
main = do
    putStrLn "START!"
    -- создание каналов
    toTree <- newChan
    toStreet <- newChan
    toJanitor <- newChan
    -- создание процессов и передача им каналов
    forkCreator $ wind toTree
    forkCreator $ tree toTree toStreet
    forkIO $ street toStreet  toJanitor StreetState {leaves = False, fantic = False}
    forkCreator $ janitor toStreet toJanitor
    -- ожидание завершения (нажатия enter)
    getLine
    return ()


