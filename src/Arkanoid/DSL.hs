{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
module Arkanoid.DSL 
  ( ArkanoidGame -- abstract
  , ArkanoidF (..) 
  , ArkanoidState
  , mkArkanoidState
  , ballPos
  , player
  , terminated
  , score
  , brickMap
  , lives
  , Env ( windowWidth
        , windowHeight
        , paddleWidth
        , paddleColor
        , ballColor
        , paddleStep
        , fps
        , logFile
        )
  , mkEnv
  , runArkanoidGame
  , printGameSnapshot
  , checkGameTerminated
  , checkGamePaused
  , waitForUnpauseCommand
  , checkPaddleBounce
  , checkWallBounce
  , checkBrickBounce
  , getCommand
  , handleCommand
  , logEvent
  , logEvents
  ) where

import Control.Lens
    ( view, over, set, makeLenses, Ixed(ix), Getting, ASetter )
import Control.Monad ( when, unless ) 
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.Extra  ( whenM )
import Control.Monad.Free   ( liftF, Free, MonadFree )
import Control.Monad.RWS
    ( MonadState(get),
      MonadWriter(tell),
      RWST(RWST),
      gets,
      asks,
      modify,
      execRWST )

import qualified Data.Bifunctor as Bi
import Data.Foldable            ( toList )
import Data.List ( partition ) 
import Data.Maybe               ( isJust )
import qualified Data.Map as M
import Data.Sequence            ( Seq )
import qualified Data.Sequence as Seq
import System.Console.ANSI      ( Color )

import Arkanoid.Types
    ( ArkanoidResult(..),
      BallPosition,
      BallVel,
      Brick(brx, hits, brw),
      BrickMap,
      Command(..),
      DifficultyLevel,
      Event(PaddleMovedRight, BallMoved, GamePaused, GameUnPaused,
            GameQuitted, PlayerWon, PlayerLost, GameTerminated, WallCollision,
            PaddleCollision, BrickCollision, PaddleMovedLeft),
      FPS,
      GamePaused,
      InputError(..),
      Lives,
      PaddleStep,
      PaddleWidth,
      PlayerPosition,
      Score,
      TerminalSize,
      WindowHeight,
      WindowWidth,
      Hits(OneH) )

-- | Data describing the state of the arkanoid game.
data ArkanoidState = ArkanoidState
  { _ballPos    :: !BallPosition            -- ^ Ball (x, y) location.
  , _ballVel    :: !BallVel                 -- ^ Ball (vx, vy) velocity given that 
                                            -- vx is pixels per second in X and 
                                            -- vy is pixels per second in Y.
  , _player     :: !PlayerPosition          -- ^ Left position in X of the player paddle.
  , _paused     :: !GamePaused              -- ^ The game is paused.
  , _terminated :: !(Maybe ArkanoidResult)  -- ^ The game is not terminated (Nothing) or 
                                            -- terminated with a 'Winner' or 'Quitted'.
  , _brickMap :: !BrickMap                  -- ^ A map with all the bricks.                         
  , _score :: !Score                        -- ^ Player score. The width of a brick broken is summed to it. 
  , _lives :: !Lives                        -- ^ The remaining number of lives.              
  } deriving (Eq, Show)

makeLenses ''ArkanoidState

-- | Smart constructor for 'ArkanoidState'.
-- It fails if it not accomplish the requirements.
mkArkanoidState :: BallPosition
                -> BallVel
                -> PlayerPosition
                -> BrickMap
                -> Lives
                -> Env
                -> Either InputError ArkanoidState 
mkArkanoidState bp@(bpx, bpy) bv@(bvx, bvy) p bricks lives' env = 
  let 
    wh          = windowWidth env
    ww          = windowWidth  env 
    playerRange = [1 .. ww - 1]
    velRange    = [-15 .. 15]
    livesRange  = [0 .. 5]

    checkPlayerPosition = p `elem` playerRange 

    checkBallPosition   =    bpx <= fromIntegral ww - 3 && bpx >= 3
                          && bpy <= fromIntegral wh - 10 && bpy >= 3

    checkBallVelocity   =    bvx `elem` velRange
                          && bvy `elem` velRange   

    checkLives          = lives' `elem` livesRange                                    
  in do 
    unless checkPlayerPosition $ throwError PlayerPositionError
    unless checkBallPosition   $ throwError BallPositionError
    unless checkBallVelocity   $ throwError BallVelocityError  
    unless checkLives          $ throwError LivesError  
    return ArkanoidState 
      { _ballPos = bp
      , _ballVel = bv
      , _player = p
      , _paused = False
      , _terminated = Nothing
      , _score = 0
      , _brickMap = bricks 
      , _lives = lives'
      }

-- | The environment.
data Env = Env 
  { windowWidth        :: !WindowWidth
  , windowHeight       :: !WindowHeight
  , paddleWidth        :: !PaddleWidth
  , paddleColor        :: !Color 
  , ballColor          :: !Color
  , paddleStep         :: !PaddleStep
  , fps                :: !FPS
  , difficulty         :: !DifficultyLevel   
  , logFile            :: !(Maybe FilePath)
  } deriving Show

-- | Smart constructor for 'Env'.
-- It fails if it not accomplish the requirements.
mkEnv :: TerminalSize
      -> WindowWidth 
      -> WindowHeight
      -> PaddleWidth 
      -> Color
      -> Color  
      -> FPS 
      -> DifficultyLevel
      -> Maybe FilePath 
      -> Either InputError Env 
mkEnv (th, tw) ww wh pw pc bc fps' d mfile = 
  let checkWindowSize = ww < tw && wh < th - 2      -- window must be smaller then terminal
      checkPaddleSize = pw <= (20 * ww) `div` 100   -- paddle must be smaller than 20% window width
      checkFPS        = fps' <= 20                  -- fps must be smaller than 20 frames per second
      checkDifficulty = d `elem` [1 .. 3]           -- Only 3 levels of difficulty, being 1 the easiest one. 
  in do 
    unless checkWindowSize $ throwError WindowSizeError
    unless checkPaddleSize $ throwError PaddleSizeError
    unless checkFPS        $ throwError FPSError
    unless checkDifficulty $ throwError DifficultyError
    return Env 
      { windowWidth  = ww
      , windowHeight = wh
      , paddleWidth = pw
      , paddleColor = pc 
      , ballColor = bc 
      , paddleStep = 2
      , fps = fps'
      , logFile = mfile
      , difficulty = d
      }

-- | The functor for our 'Free' monad.
-- This instructions are interpreted by the interpreters (pure or IO interpreter)
data ArkanoidF next 
    = GetCommand (Command -> next)         -- ^ Gets a 'Command'.
    | PrintGameSnapshot ArkanoidState next -- ^ Exposes ArkanoidState in order to be "printed" somehow. 
    deriving Functor

-- | The ArkanoidGame monad (abstract).
-- Reader for reading the 'Env'.
-- Writer for writing the multiple 'Event's on a log.
-- State for keeping the state of the game 'ArkanoidState'.
-- Free for defining actions which need to be interpreted depending on the context 
-- (IO for printing on the terminal or pure for testing)   
newtype ArkanoidGame a = ArkanoidGame { runPG :: RWST Env (Seq Event) ArkanoidState (Free ArkanoidF) a  }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadFree ArkanoidF
    )

-- | Run a game given an 'Env' and an initial state 'ArkanoidState'.
-- It returns a 'Free' monad with the actions to be interpreted along with 
-- the state, 'ArkanoidState', and the event log, '[Event]' as the result of
-- the 'Free' monad.
runArkanoidGame :: Env 
                -> ArkanoidState
                -> ArkanoidGame a
                -> Free ArkanoidF (ArkanoidState, [Event])
runArkanoidGame env initialState game = 
  Bi.second toList <$> execRWST (runPG game) env initialState

-- | Print a snapshot of the 'ArkanoidState'.
-- Notice that lift the abstract action of 'PrintGameSnapshot'.
-- The interpreters will know what to do with this. For example,
-- an IO interpreter would print on the terminal. 
printGameSnapshot :: ArkanoidGame ()
printGameSnapshot = do
  state <- ArkanoidGame get
  liftF $ PrintGameSnapshot state ()

-- | Check whether the game is terminated.
checkGameTerminated :: ArkanoidGame Bool
checkGameTerminated = isJust <$> isGameTerminated

-- | Check whether the game is paused.
checkGamePaused :: ArkanoidGame Bool
checkGamePaused = readStateVar paused

-- | Block until an un'Pause' command is received.
-- Note that the pause action is toggled (same command for pausing and unpausing).
-- This action must be used after pausing the game.
-- Commands received after a pause action wil be ignored. Once an unpause ('Pause') command
-- is received commands will be handle normally.
waitForUnpauseCommand :: ArkanoidGame Command
waitForUnpauseCommand = do 
  cmd <- getCommand
  case cmd of
    Pause -> return Pause
    _     -> waitForUnpauseCommand

-- | Get a command and it returns it lifting the abstract action
-- 'GetCommand' which will be interpreted by the interpreters. 
getCommand :: ArkanoidGame Command
getCommand = liftF $ GetCommand id

-- | Handle a command modifying the game state.
handleCommand :: Command -> ArkanoidGame ()
handleCommand cmd = do
  case cmd of 
    MoveLeftPaddle  -> movePaddleLeft
    MoveRightPaddle -> movePaddleRight 
    Tick            -> moveBall
    Quit            -> quitGame
    Pause           -> tooglePause 

-- | Move the ball from the ballVel (pixels per second) and FPS.
moveBall :: ArkanoidGame ()
moveBall = do
  fps'         <- readEnvVar fps
  (x,  y)      <- readStateVar ballPos
  (vx, vy)     <- readStateVar ballVel
  let frameTime = recip $ fromIntegral fps'
      x'        = x + (fromIntegral vx * frameTime)
      y'        = y + (fromIntegral vy * frameTime)

  setStateVar ballPos (x', y')
  logEvent $ BallMoved (x', y')

-- | Pause or unpause the game.
tooglePause :: ArkanoidGame ()
tooglePause = do
  overStateVar paused not
  isPaused <- readStateVar paused
  logEvent $ if isPaused then GamePaused else GameUnPaused

-- | Quit the game.
quitGame :: ArkanoidGame ()
quitGame = do 
  setStateVar terminated (Just Quitted)
  logEvent GameQuitted  

-- | Check whether the game is terminated and return the result. 
-- There are 2 cases:
-- 1. The game has been quitted.
-- 2. The ball crossed the bottom line 
-- without being intercepted by the paddle.    
-- 3. Player has won the game.
isGameTerminated :: ArkanoidGame (Maybe ArkanoidResult)
isGameTerminated = do 
  isTerminated <- readStateVar terminated
  lives'       <- readStateVar lives
  bricks       <- readStateVar brickMap
  case isTerminated of 
    Just result -> return $ Just result
    Nothing     -> do
      if all null bricks   -- brickMap empty ?
        then do 
          score' <- readStateVar score
          let result = Just $ PlayerWin score'
          setStateVar terminated result
          logEvents [PlayerWon score', GameTerminated]
          return result
        else do  
          let checkBallPos y vy windowWidth' = signum vy == 1 && y >= windowWidth'
          (_, y)        <- readStateVar ballPos
          (vx, vy)      <- readStateVar ballVel
          score'        <- readStateVar score
          windowHeight' <- readEnvVar windowHeight
          if checkBallPos y vy (fromIntegral windowHeight')
            then if lives' >= 1 
                then do
                  p  <- readStateVar player 
                  pw <- readEnvVar paddleWidth
                  let bpy = fromIntegral windowHeight' - 2
                      bpx = fromIntegral $ p + pw - 1
                      bv = (vx, negate . abs $ vy)
                  setStateVar ballPos (bpx, bpy)
                  setStateVar ballVel bv
                  overStateVar lives pred
                  return Nothing 
                else do   
                  let result = Just $ PlayerLose score'
                  setStateVar terminated result
                  logEvents [PlayerLost score', GameTerminated]
                  return result
            else return Nothing  

-- | Check whether a wall bounce has happened, updating the Arkanoid 
-- state if so.
checkWallBounce :: ArkanoidGame ()
checkWallBounce =  
  let checkCollision x y vx vy windowWidth' 
        | signum vy == -1 && y < 1                 = Just (vx, -vy)
        | signum vx ==  1 && x > windowWidth' - 1  = Just (-vx, vy) 
        | signum vx == -1 && x < 1                 = Just (-vx, vy) 
        | otherwise                                = Nothing      
  in do
    (x, y)       <- readStateVar ballPos
    (vx, vy)     <- readStateVar ballVel
    windowWidth' <- readEnvVar windowWidth
    
    case checkCollision x y vx vy (fromIntegral windowWidth') of
      Just newVel -> do
        setStateVar ballVel newVel
        logEvent $ WallCollision (vx, vy) newVel
      Nothing     -> return () 
      
-- | Check whether a paddle bounce has happened, updating the game 
-- state if so.
checkPaddleBounce :: ArkanoidGame ()
checkPaddleBounce = do
  ballPos'  <- readStateVar ballPos
  p         <- readStateVar player
  (vx, vy)  <- readStateVar ballVel
  collision <- paddleCollision ballPos' (vx, vy) p
  case collision of
    Just newVel -> do
      setStateVar ballVel newVel
      logEvent $ PaddleCollision (vx, vy) newVel
    Nothing            -> return ()

-- | Check whether a paddle has happened returning the collision if so.
-- A collision at the center does not correct the ball velocity in X (0)
-- A collision at the upper side corrects the ball velocity with factor (-1)
-- A collision at the lower side corrects the ball velocity with factor (+1)
paddleCollision :: BallPosition 
                -> BallVel 
                -> PlayerPosition 
                -> ArkanoidGame (Maybe BallVel)
paddleCollision (x, y) (vx, vy) p = 
  let paddleCollisionInY windowHeight' = y > fromIntegral windowHeight' - 2
    
      paddleCollisionInX paddleWidth'  = let p' = fromIntegral p 
                                         in x >= p' && x < p' + fromIntegral paddleWidth'
      
      velCorrection paddleWidth'       = let halfPos = p + paddleWidth' `div` 2 
                                         in case x `compare` fromIntegral halfPos of 
                                              EQ -> 0   -- center collision 
                                              LT -> -1  -- upper side collision
                                              GT -> 1   -- lower side collision
  in do
    windowHeight' <- readEnvVar windowHeight
    paddleWidth'  <- readEnvVar paddleWidth
    if signum vy == 1 && paddleCollisionInY windowHeight' && paddleCollisionInX paddleWidth'
      then let vx' = vx + velCorrection paddleWidth'
           in return $ Just (vx', -vy) 
      else return Nothing

-- | Check whether it was a collision with a brick, changing ball velocity
-- and removing the brick from the brickMap.   
checkBrickBounce :: ArkanoidGame ()
checkBrickBounce = do 
  (x, y)   <- readStateVar ballPos
  (vx, vy) <- readStateVar ballVel
  bricks   <- readStateVar brickMap
  let line       = ceiling y
      mbrickLine = M.lookup line bricks 
  case mbrickLine of 
    Nothing        -> return ()
    Just brickLine -> do
      let collision = flip partition brickLine $ \b ->
            let brx' = fromIntegral (brx b)
                brw' = fromIntegral (brw b)
            in x >= brx' && x <= brx' + brw'
      case collision of 
        ([brick], noCollisionBricks) -> do
          let newVel = (vx, -vy)
              brickLine' = noCollisionBricks ++ 
                [ brick { hits = pred $ hits brick } | hits brick > OneH ] 

          setStateVar ballVel newVel
          setStateVar (brickMap . ix line) brickLine'
          logEvent $ BrickCollision (vx, vy) newVel 
          when (hits brick == OneH) $ overStateVar score (+ brw brick)

        ([],  _)       -> return ()  
        (_:_, _)       -> error "2 o more collisions can never happen"  

-- | Move up the player paddle when it is possible. 
-- Check the upper limit of the top wall.      
movePaddleLeft :: ArkanoidGame ()
movePaddleLeft = do
  p <- readStateVar player
  movePaddle p (set player) (-) validatePaddleLeft (PaddleMovedLeft p)

-- | Move down the player paddle when it is possible. 
-- Check the lower limit of the bottom wall.
movePaddleRight :: ArkanoidGame ()
movePaddleRight = do
  p <- readStateVar player
  movePaddle p (set player) (+) validatePaddleRight (PaddleMovedRight p)

type PlayerPosModifier       = PlayerPosition -> ArkanoidState -> ArkanoidState
type PlayerPositionOp        = PlayerPosition -> PaddleStep -> PlayerPosition
type PlayerPositionValidator = PlayerPosition -> ArkanoidGame Bool

-- | Helper function for moving a paddle.
movePaddle :: PlayerPosition 
           -> PlayerPosModifier 
           -> PlayerPositionOp 
           -> PlayerPositionValidator 
           -> (PlayerPosition -> Event) 
           -> ArkanoidGame ()
movePaddle oldPos modifier operator validator eventF = do
    paddleStep' <- readEnvVar paddleStep
    let newPos = oldPos `operator` paddleStep' 
    whenM (validator newPos) $ do 
      ArkanoidGame $ modify $ modifier newPos
      logEvent (eventF newPos)

-- | Validate a player position at the board left side.
-- Notice that 'PlayerPosition' corresponds with 
-- the left position in X of the player paddle.
validatePaddleLeft :: PlayerPosition -> ArkanoidGame Bool
validatePaddleLeft newPos = return $ newPos >= 1

-- | Validate a player position at the board right side.
-- Notice that 'PlayerPosition' corresponds with 
-- the left position in X of the player paddle.
validatePaddleRight :: PlayerPosition -> ArkanoidGame Bool
validatePaddleRight newPos = do
  paddleWidth' <- readEnvVar paddleWidth
  windowWidth' <- readEnvVar windowWidth
  return $ newPos + paddleWidth' <= windowWidth'  

-- | Set a value 'a' into 'ArkanoidState' given a 'ASetter' lens.
setStateVar :: ASetter ArkanoidState ArkanoidState a a -> a -> ArkanoidGame () 
setStateVar setter = ArkanoidGame . modify . set setter 

-- | Read a value 'a' from 'ArkanoidState' given a 'Getting' lens.
readStateVar :: Getting a ArkanoidState a -> ArkanoidGame a
readStateVar = ArkanoidGame . gets . view

-- | Apply a function over a 'ArkanoidState' value given a 'ASetter' lens.
overStateVar :: ASetter ArkanoidState ArkanoidState a b -> (a -> b) -> ArkanoidGame () 
overStateVar setter = ArkanoidGame . modify . over setter

-- | Read a value from the environment.
readEnvVar :: (Env -> a) -> ArkanoidGame a
readEnvVar = ArkanoidGame . asks 

-- | Write an event on the event log.
logEvent :: Event -> ArkanoidGame ()
logEvent = ArkanoidGame . tell . Seq.singleton

-- | Write an event list on the event log.
logEvents :: [Event] -> ArkanoidGame ()
logEvents = ArkanoidGame . tell . Seq.fromList
