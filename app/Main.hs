{-# LANGUAGE NumericUnderscores #-}
module Main(main) where
  
-- import Control.Concurrent.Chan ( newChan, Chan )
import Control.Exception ( IOException, finally, handle )
import Control.Monad ( when )
import Control.Monad.Extra ( unlessM, whenM )

import Control.Concurrent.STM.TChan

import Arkanoid.Draw ( printer, restoreTerminal )
import Arkanoid.DSL
    ( checkBrickBounce,
      checkGamePaused,
      checkGameTerminated,
      checkPaddleBounce,
      checkWallBounce,
      getCommand,
      handleCommand,
      logEvent,
      printGameSnapshot,
      waitForUnpauseCommand,
      ArkanoidGame,
      ArkanoidState,
      Env(fps, windowHeight, logFile) )
import Arkanoid.IOInterpreter
    ( inputHandler, runPongGameInIO, ticker )
import Arkanoid.Types ( Command(Tick), Event(GameStarted) )
import Cli ( parseArgsIO )
import Utils ( race4_ )

main :: IO ()
main = handle logIOException $ do
  (env, initialState) <- parseArgsIO 
  chanInput           <- newTChanIO
  chanState           <- newTChanIO

  let fps' = fps env
      frameInterval = 1_000_000 `div` fps' 

  race4_ (inputHandler chanInput)          
         -- Fork a thread for getting the input commands.
         (ticker frameInterval chanInput)  
         -- Fork a thread for running the clock.
         (printer env chanState `finally` restoreTerminal (windowHeight env))
         -- Fork a thread for drawing the game snapshots.
         (gameRunner chanInput chanState env initialState)
         -- Fork a thread for running the game.

-- | Run a game in the IO monad.
gameRunner :: TChan Command -> TChan ArkanoidState -> Env -> ArkanoidState -> IO () 
gameRunner chanInput chanState env initialState = do
  (_result, events) <- runPongGameInIO chanInput chanState env initialState game
  mapM_ (writeEventLog events) $ logFile env

-- | The game to run.
game :: ArkanoidGame ()
game = 
  let go = do 
        unlessM checkGameTerminated $ do
          whenM checkGamePaused $ do waitForUnpauseCommand >>= handleCommand
          checkWallBounce
          checkPaddleBounce
          checkBrickBounce
          cmd <- getCommand 
          handleCommand cmd
          when (cmd == Tick) printGameSnapshot
          go
        printGameSnapshot
  in logEvent GameStarted >> go

-- | Print an IOException.
logIOException :: IOException -> IO ()  
logIOException = print

-- | Write the event log on a file.
writeEventLog :: [Event] -> FilePath -> IO ()
writeEventLog events file = writeFile file . unlines . map show $ events 
