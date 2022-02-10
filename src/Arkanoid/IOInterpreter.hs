module Arkanoid.IOInterpreter 
  ( runPongGameInIO
  , inputHandler
  , ticker
  ) where

import Control.Concurrent.STM
    ( atomically, readTChan, writeTChan, TChan )
import Control.Concurrent ( threadDelay )
import Control.Lens ( view )
import Control.Monad ( forever ) 
import Control.Monad.Free ( Free(..) )
import System.IO
    ( hSetBuffering, hSetEcho, stdin, BufferMode(NoBuffering) )

import Arkanoid.DSL
    ( ArkanoidState,
      ArkanoidGame,
      ArkanoidF(..),
      Env,
      terminated,
      runArkanoidGame )
import Arkanoid.Types ( Event, Command(..), ArkanoidResult ) 

-- | Run a game in the IO monad returning the game result and the event log.
-- The side effects (IO) consist of:
-- 1. Reading inputs from stdin and writing them on the command channel.    
-- 2. Writing 'ArkanoidState' snapshots on the state channel.    
runPongGameInIO :: TChan Command
                -> TChan ArkanoidState 
                -> Env
                -> ArkanoidState
                -> ArkanoidGame a
                -> IO (ArkanoidResult, [Event])
runPongGameInIO chanInput chanState env initialState game = do
  (finalState, events) <- interpretInMonadIO chanInput chanState $ runArkanoidGame env initialState game
  case view terminated finalState of
    Nothing     -> error "This can never happen"  -- A game always returns a result
    Just result -> return (result, events)

-- | Interpret the 'Free' actions in IO monad.
-- 'GetCommand'        --> Read a character from stdin.
-- 'PrintGameSnapshot' --> Write the state snapshot on a state channel.     
interpretInMonadIO :: TChan Command -> TChan ArkanoidState -> Free ArkanoidF next -> IO next
interpretInMonadIO _chanInput _chanState (Pure r)                             = return r
interpretInMonadIO chanInput  chanState  (Free (GetCommand nextFun))          = do 
  cmd <- readCommandIO chanInput
  interpretInMonadIO chanInput chanState $ nextFun cmd
interpretInMonadIO chanInput  chanState  (Free(PrintGameSnapshot state next)) = do 
  writeGameSnapshotIO chanState state 
  interpretInMonadIO chanInput chanState next

-- | Read characters from stdin and write them on the commands channel.
inputHandler :: TChan Command -> IO ()
inputHandler chanInput = forever $ do
  hSetBuffering stdin NoBuffering -- disable input buffering
  hSetEcho stdin False
  c <- getChar
  case c of 
    'z' -> atomically $ writeTChan chanInput MoveLeftPaddle
    'm' -> atomically $ writeTChan chanInput MoveRightPaddle 
    'p' -> atomically $ writeTChan chanInput Pause
    'q' -> atomically $ writeTChan chanInput Quit
    _   -> return ()

-- | Write a 'Tick' command on the commands channel periodically
-- given a frame interval interval which is calculated by the caller 
-- depending on FPS param of the game.
-- 'ticker' would be the clock of the ArkanoidGame system. 
ticker :: Int -> TChan Command -> IO ()
ticker frameInterval chanInput = 
  let go = do 
        threadDelay frameInterval 
        atomically $ writeTChan chanInput Tick
        go 
  in go 

-- | Write a 'ArkanoidState' snapshot on the 'ArkanoidState' channel.
writeGameSnapshotIO :: TChan ArkanoidState -> ArkanoidState -> IO ()
writeGameSnapshotIO chanState = atomically . writeTChan chanState 

-- | Read a 'Command' from the commands channel.
readCommandIO :: TChan Command -> IO Command
readCommandIO = atomically . readTChan
