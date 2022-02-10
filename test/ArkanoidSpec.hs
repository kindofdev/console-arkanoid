module Main (main) where

import Control.Lens ( view )
import Control.Monad.Extra ( unlessM, whenM )
import Data.Either.Combinators ( rightToMaybe )
import qualified Data.Map as M
import System.Console.ANSI ( Color(White, Red, Yellow) )
import Test.Tasty.HUnit ( testCase, (@?) )
import Test.Tasty ( TestTree, defaultMain, testGroup )

import Arkanoid.DSL
    ( ArkanoidState,
      ArkanoidGame,
      Env,
      player,
      mkArkanoidState,
      mkEnv,
      printGameSnapshot,
      checkGameTerminated,
      checkGamePaused,
      waitForUnpauseCommand,
      getCommand,
      handleCommand,
      checkWallBounce,
      checkPaddleBounce,
      checkBrickBounce,
      logEvent )
import Arkanoid.PureInterpreter ( runPongGamePure, PureResult )
import Arkanoid.Types
    ( InputError,
      Event(BrickCollision, GameStarted, WallCollision, PaddleCollision,
            BallMoved, PaddleMovedRight),
      Command,
      ArkanoidResult(..),
      Lives,
      Brick(Brick),
      BrickMap,
      Hits(OneH),
      BallVel,
      BallPosition,
      PlayerPosition )
import CommandSeq
    ( pauseUnpauseAndMore, playerMoveAndQuit, ticksAndQuit )
import Utils ( first, second, seqLast, third )

main :: IO ()
main = defaultMain tests 

tests :: TestTree
tests = testGroup "Arkanoid Tests" [unitTests]

-- Run multiple test cases in a pure fashion using the 'PureInterpreter' module. 
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "A player can quit a game" $
      let cmds   = ticksAndQuit 50
          bp     = (50, 15)
          bv     = (10, 0)
          p      = 1
          lives' = 0
          result = runGame cmds (inputParams bp bv p aCornerBrick lives') game
      in isQuitted result @? "" 

  , testCase "A player can move the paddle" $ 
      let cmds   = playerMoveAndQuit
          bp     = (50, 15) 
          bv     = (10, 0)
          p      = 1
          lives' = 0
          result = runGame cmds (inputParams bp bv p aCornerBrick lives') game
      in inputCmdsModifiesPlayerPosition (p + 8) result @? "" 

  , testCase "A player can win a game" $
      let cmds   = ticksAndQuit 50
          bp     = (50, 15) 
          bv     = (0, -10)
          p      = 1
          bricks = M.fromList [(10, [Brick 48 5 White OneH])]
          lives' = 0
          result = runGame cmds (inputParams bp bv p bricks lives') game
      in playerWins result @? "" 

  , testCase "A player can lose a game" $
      let cmds   = ticksAndQuit 150
          bp     = (50, 15) 
          bv     = (0, 10)
          p      = 1
          lives' = 0
          result = runGame cmds (inputParams bp bv p aCornerBrick lives') game
      in playerLoses result @? ""   

  , testCase "A top wall collision modifies ball velocity in Y" $
      let cmds       = ticksAndQuit 50
          bp         = (50, 15)
          bv         = (1, -10)
          bvExpected = (1, 10) 
          p          = 1
          lives'     = 0
          result     = runGame cmds (inputParams bp bv p aCornerBrick lives') game
      in wallCollisionModifiesVelocity bv bvExpected result @? ""   

  , testCase "A left wall collision modifies ball velocity in X" $
      let cmds       = ticksAndQuit 50
          bp         = (50, 15)
          bv         = (-10, 1)
          bvExpected = (10, 1) 
          p          = 1
          lives'     = 0
          result     = runGame cmds (inputParams bp bv p aCornerBrick lives') game
      in wallCollisionModifiesVelocity bv bvExpected result @? ""   

  , testCase "A right wall collision modifies ball velocity in X" $
      let cmds       = ticksAndQuit 200
          bp         = (50, 15)
          bv         = (10, 1)
          bvExpected = (-10, 1) 
          p          = 1
          lives'     = 0 
          result     = runGame cmds (inputParams bp bv p aCornerBrick lives') game
      in wallCollisionModifiesVelocity bv bvExpected result @? ""    

  , testCase "A paddle collision on the center only modifies ball velocity in Y" $
      let cmds       = ticksAndQuit 150
          bp         = (50, 15)
          bv         = (0, 10)
          bvExpected = (0, -10) 
          p          = 48
          lives'     = 0
          result     = runGame cmds (inputParams bp bv p aCornerBrick lives') game
      in paddleCollisionModifiesVelocity bv bvExpected result @? ""   

  , testCase "A paddle collision on the left side modifies ball velocity in X and Y" $
      let cmds       = ticksAndQuit 150
          bp         = (50, 15)
          bv         = (0, 10)
          bvExpected = (-1, -10) 
          p          = 49
          lives'     = 0
          result     = runGame cmds (inputParams bp bv p aCornerBrick lives') game
      in paddleCollisionModifiesVelocity bv bvExpected result @? ""     

  , testCase "A paddle collision on the right side modifies ball velocity in X and Y" $
      let cmds       = ticksAndQuit 150
          bp         = (50, 15)
          bv         = (0, 10)
          bvExpected = (1, -10) 
          p          = 47
          lives'     = 0
          result     = runGame cmds (inputParams bp bv p aCornerBrick lives') game
      in paddleCollisionModifiesVelocity bv bvExpected result @? ""  

  , testCase "A player can pause a game" $ 
      let cmds   = pauseUnpauseAndMore
          bv     = (10, 0)
          bp     = (50, 15)
          p      = 13
          lives' = 0
          result = runGame cmds (inputParams bp bv p aCornerBrick lives') game
      in (pauseStopsBallUntilUnpause 20 result && pauseMakesCommandsIgnored result) @? ""  

  , testCase "A ball going up can collision with a brick only modifying ball velocity in Y" $
      let cmds       = ticksAndQuit 50
          bp         = (50, 15)
          bv         = (0, -10)
          bvExpected = (0, 10) 
          p          = 1
          bricks     = M.fromList [(10, [Brick 48 5 White OneH])]
          lives'     = 0
          result     = runGame cmds (inputParams bp bv p bricks lives') game
      in brickCollisionModifiesVelocity bv bvExpected result @? "" 

  , testCase "A ball going down can collision with a brick only modifying ball velocity in Y" $
      let cmds       = ticksAndQuit 50
          bp         = (50, 15)
          bv         = (0, 10)
          bvExpected = (0, -10) 
          p          = 1
          bricks     = M.fromList [(20, [Brick 48 5 White OneH])]
          lives'     = 0
          result     = runGame cmds (inputParams bp bv p bricks lives') game
      in brickCollisionModifiesVelocity bv bvExpected result @? ""   

  , testCase "A ball can't collision with a brick if it's out of scope" $
      let cmds       = ticksAndQuit 50
          bp         = (50, 15)
          bv         = (0, 10)
          p          = 1
          bricks     = M.fromList [(20, [Brick 10 5 White OneH])]
          lives'     = 0
          result     = runGame cmds (inputParams bp bv p bricks lives') game
      in noBrickCollisionEvent result @? ""   
  ]

game :: ArkanoidGame ()
game = 
  let go = do 
        unlessM checkGameTerminated $ do
          whenM checkGamePaused $ do waitForUnpauseCommand >>= handleCommand
          checkPaddleBounce
          checkWallBounce
          checkBrickBounce 
          getCommand >>= handleCommand
          printGameSnapshot
          go
  in logEvent GameStarted >> go

runGame :: [Command] 
        -> Either InputError (Env, ArkanoidState) 
        -> ArkanoidGame a 
        -> PureResult  
runGame cmds eparams game' = 
  let (env, state) = either (error . show) id eparams
  in runPongGamePure cmds env state game'

inputParams :: BallPosition 
            -> BallVel 
            -> PlayerPosition 
            -> BrickMap 
            -> Lives
            -> Either InputError (Env, ArkanoidState)
inputParams bp bv p bricks lives' = do 
  let tw     = 251
      th     = 153
      ww     = 250
      wh     = 150
      pw     = 5
      fps'   = 10 
  env   <- mkEnv (th, tw) ww wh pw Red Yellow fps' 1 Nothing 
  state <- mkArkanoidState bp bv p bricks lives' env
  return (env, state)

getArkanoidGameResult :: PureResult -> Maybe ArkanoidResult
getArkanoidGameResult result = first <$> rightToMaybe result

getArkanoidGameEvents :: PureResult -> Maybe [Event]
getArkanoidGameEvents result = second <$> rightToMaybe result

getArkanoidLastState :: PureResult -> Maybe ArkanoidState
getArkanoidLastState result = rightToMaybe result >>= seqLast . third
    
inputCmdsModifiesPlayerPosition :: PlayerPosition -> PureResult -> Bool
inputCmdsModifiesPlayerPosition p = 
    any (\lastState -> view player lastState == p) . getArkanoidLastState 

wallCollisionModifiesVelocity :: BallVel -> BallVel -> PureResult -> Bool
wallCollisionModifiesVelocity bv bv' = 
  any (WallCollision bv bv' `elem`) . getArkanoidGameEvents

paddleCollisionModifiesVelocity :: BallVel -> BallVel -> PureResult -> Bool
paddleCollisionModifiesVelocity bv bv' =
  any (PaddleCollision bv bv' `elem`) . getArkanoidGameEvents 

brickCollisionModifiesVelocity :: BallVel -> BallVel -> PureResult -> Bool
brickCollisionModifiesVelocity bv bv' =
  any (BrickCollision bv bv' `elem`) . getArkanoidGameEvents 

pauseStopsBallUntilUnpause :: Int -> PureResult -> Bool  
pauseStopsBallUntilUnpause n result = 
   any (\events -> length [event | event @ (BallMoved _) <- events] == n)
       (getArkanoidGameEvents result)
  
pauseMakesCommandsIgnored :: PureResult -> Bool 
pauseMakesCommandsIgnored result = 
  any (\events -> null [event | event @ (PaddleMovedRight _ _) <- events])
      (getArkanoidGameEvents result)

isQuitted :: PureResult -> Bool
isQuitted result = getArkanoidGameResult result == Just Quitted

playerLoses :: PureResult -> Bool 
playerLoses result = getArkanoidGameResult result == Just (PlayerLose 0)

playerWins :: PureResult -> Bool 
playerWins result = getArkanoidGameResult result == Just (PlayerWin 5)

aCornerBrick :: BrickMap 
aCornerBrick = M.fromList [(1, [Brick 1 1 White OneH])] 

noBrickCollisionEvent :: PureResult -> Bool 
noBrickCollisionEvent result = 
  any (\events -> null [event | event @ (BrickCollision _ _) <- events])
      (getArkanoidGameEvents result)  



-- main :: IO ()
-- main = do
--   let cmds   = ticksAndQuit 50
--       bp     = (50, 15) 
--       bv     = (0, -10)
--       p      = 1
--       bricks = M.fromList [(10, [Brick 48 5 White OneH])]
--       lives' = 0
--       (env, state) = E.fromRight undefined $ inputParams bp bv p bricks lives'
--   case runPongGamePure cmds env state game of
--     Left error'                    -> putStrLn $ "ERROR: " <> show error'
--     Right (result, events, states) -> do
--       putStrLn "GAME RESULT"
--       print result
--       putChar '\n'
--       putStrLn "GAME STATES"
--       forM_ states print
--       putChar '\n'
--       putStrLn "EVENT LOG"
--       forM_ events print  
