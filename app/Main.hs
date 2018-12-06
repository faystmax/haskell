module Main where

import Control.Monad (forever)

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import System.Random

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

street :: Chan [Char] -> Chan [Char] -> Chan [Char] -> StreetState -> IO()
street toStreet toJanitor toPolice state = do
    print $ "Street: Waiting for something. State = " ++ show state
    received <- readChan toStreet
    putStrLn $ "Street: received = " ++ received
    parseActionAnswer received toJanitor toPolice state
    street toStreet toJanitor toPolice $ parseStateAnswer received state

parseStateAnswer :: [Char] -> StreetState -> StreetState
parseStateAnswer "leaves" state = state {leaves = True}
parseStateAnswer "clean" state = state {leaves = False, fantic = False}
parseStateAnswer "fantic" state = state {fantic = True}
parseStateAnswer answer state = state

parseActionAnswer :: [Char] -> Chan [Char] -> Chan [Char] -> StreetState -> IO()
parseActionAnswer "isClean" toJanitor toPolice StreetState{leaves = False , fantic = False} =  writeChan toJanitor "yes"
parseActionAnswer "isClean" toJanitor toPolice state =  writeChan toJanitor "no"
parseActionAnswer "isFantic" toJanitor toPolice StreetState{fantic = False} =  writeChan toPolice "no"
parseActionAnswer "isFantic" toJanitor toPolice StreetState{fantic = True} =  writeChan toPolice "yes"
parseActionAnswer answer toJanitor toPolice state =  return ()


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

police :: Chan [Char] ->  Chan [Char] -> Chan [Char]  -> IO()
police toStreet toStudent toPolice = do
    print $ "Police: Sending isFantic to Street "
    writeChan toStreet "isFantic"
    received <- readChan toPolice
    putStrLn $ "Police: received = " ++ received
    parsePoliceAnswer received toStudent
    threadDelay 10000000

parsePoliceAnswer :: [Char] -> Chan [Char] -> IO()
parsePoliceAnswer "no"  toStudent =  do
    writeChan toStudent "penalty"
    putStrLn $ "penalty "
parsePoliceAnswer "yes" toStudent =  return ()

student :: Chan [Char] -> Chan [Char]  -> IO()
student toStreet toStudent = do
    print $ "Student: Sending fantic to Street "
    writeChan toStreet "fantic"
    waitPenalty 10 toStudent

--    parseStudentAnswer received


waitPenalty 0 toStudent = return ()
waitPenalty n toStudent = do
      received <-  readChan  toStudent
      case received of
          "penalty" -> do
              putStrLn $ "received penalty "
              return ()
          _ -> do
              putStrLn "no message received"
              threadDelay 1000000
              waitPenalty (n-1) toStudent

--parseStudentAnswer (Just "penalty") =   print $ "Student: received penalty "
--parseStudentAnswer answer =   print $ "Student: Got nothing "

forkCreator action = do
    forkIO $ forever action

main = do
    putStrLn "START!"
    -- создание каналов
    toTree <- newChan
    toStreet <- newChan
    toJanitor <- newChan
    toPolice <- newChan
    toStudent <- newChan
    -- создание процессов и передача им каналов
    forkCreator $ wind toTree
    forkCreator $ tree toTree toStreet
    forkIO $ street toStreet toJanitor toPolice StreetState {leaves = False, fantic = False}
    forkCreator $ janitor toStreet toJanitor
    forkCreator $ police toStreet toStudent toPolice
    forkCreator $ student toStreet toStudent
    -- ожидание завершения (нажатия enter)
    getLine
    return ()


