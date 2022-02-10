module Arkanoid.PureInterpreter
  ( runPongGamePure
  , InvalidGame
  , PureResult
  ) where

import Control.Lens ( view )
import Control.Monad.Except
    ( runExcept, MonadError(throwError), Except )
import Control.Monad.Free ( Free(..) )
import Control.Monad.Writer
    ( MonadWriter(tell), WriterT(runWriterT) )
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Arkanoid.DSL
    ( ArkanoidState,
      ArkanoidGame,
      ArkanoidF(..),
      Env,
      terminated,
      runArkanoidGame )
import Arkanoid.Types ( Command, Event, ArkanoidResult )

-- | A monad for interpreting a game in a pure fashion. 
-- Writer for keeping track of 'ArkanoidState' snapshots (a pure printing)
-- Except because the interpretation of a game can fail due to
-- a sequence of commands invalid: an execution of a game with no result.    
type PureInterM a = WriterT (Seq ArkanoidState) (Except InvalidGame) a 

-- | An invalid execution of a game. 
data InvalidGame = InvalidGame deriving (Eq, Show)

-- | The result of running a game with the pure interpreter 'PureInterM'.
-- It can fails with a 'InvalidGame' or succeeds with a ArkanoidResult,   
-- the event log and a sequence of all the snapshots of ArkanoidState. 
type PureResult = Either InvalidGame (ArkanoidResult, [Event], Seq ArkanoidState) 

-- | Run a game in a pure fashion given a sequence of 'Command's. 
runPongGamePure :: [Command]
                -> Env
                -> ArkanoidState
                -> ArkanoidGame a
                -> PureResult
runPongGamePure cmds env initialState game = runExcept $ do 
  ((finalState, events), states) <- runWriterT $ interpretInPureInterpreter cmds $ runArkanoidGame env initialState game
  case view terminated finalState of 
    Nothing     -> error "This can never happen" -- A game always returns a result
    Just result -> return (result, events, states) 

-- | Interpret the 'Free' actions in 'PureInterM' monad.
-- 'GetCommand'        --> Read a character from stdin.
-- 'PrintGameSnapshot' --> Write the state snapshot on a state channel.                                  
interpretInPureInterpreter :: [Command] -> Free ArkanoidF next -> PureInterM next
interpretInPureInterpreter _cmds      (Pure r)                              = return r
interpretInPureInterpreter cmds       (Free (PrintGameSnapshot state next)) = do tell $ Seq.singleton state
                                                                                 interpretInPureInterpreter cmds next 
interpretInPureInterpreter (cmd:cmds) (Free (GetCommand nextFun))           = interpretInPureInterpreter cmds $ nextFun cmd
interpretInPureInterpreter []         (Free (GetCommand _      ))           = throwError InvalidGame
